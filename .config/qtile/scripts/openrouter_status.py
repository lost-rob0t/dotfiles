#!/usr/bin/env python3
"""Collect trustworthy OpenRouter rate, balance, token, spend, and history telemetry."""

from __future__ import annotations

import argparse
import concurrent.futures
import fcntl
import json
import os
import sqlite3
import subprocess
import sys
import time
import urllib.error
import urllib.request
from dataclasses import asdict, dataclass, replace
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

QTILE_DIR = Path(__file__).resolve().parents[1]
if str(QTILE_DIR) not in sys.path:
    sys.path.insert(0, str(QTILE_DIR))

import openrouter_history as history

API_BASE = "https://openrouter.ai/api/v1"
AI_ICON = "\U000F09D1"

CACHE_TTL_SECONDS = 0.75
RATE_WINDOW_SECONDS = 60
USAGE_REFRESH_SECONDS = 30
BALANCE_REFRESH_SECONDS = 30
REQUEST_TIMEOUT_SECONDS = 5
DEFAULT_MAX_TPM = 10_000_000
BACKFILL_INTERVAL_SECONDS = 6 * 60 * 60
BACKFILL_DAYS = 365
BACKFILL_FINE_DAYS = 7
DAEMON_POLL_SECONDS = 1.0
DAEMON_HEARTBEAT_STALE_SECONDS = 5.0
COLLECTOR_KEYS = (
    "collector_pid",
    "collector_parent_pid",
    "collector_heartbeat",
    "collector_error",
    "collector_stale",
)


@dataclass(frozen=True)
class Status:
    input_tokens_per_minute: int
    output_tokens_per_minute: int
    balance_usd: float | None = None
    window_end: str | None = None
    tokens_month: int | None = None
    tokens_week: int | None = None
    tokens_day: int | None = None
    tokens_hour: int | None = None
    tokens_minute: int | None = None
    tokens_year: int | None = None
    spend_month: float | None = None
    spend_week: float | None = None
    spend_day: float | None = None
    spend_year: float | None = None
    usage_fetched_at: str | None = None
    balance_fetched_at: str | None = None
    usage_error: str | None = None
    balance_error: str | None = None
    last_error: str | None = None

    @property
    def total_tokens_per_minute(self) -> int:
        return self.input_tokens_per_minute + self.output_tokens_per_minute


def compact_count(value: int | float) -> str:
    value = float(value)
    for suffix, divisor in (("B", 1_000_000_000), ("M", 1_000_000), ("k", 1_000)):
        if abs(value) >= divisor:
            rendered = f"{value / divisor:.1f}".rstrip("0").rstrip(".")
            return f"{rendered}{suffix}"
    return str(int(round(value)))


def render(status: Status, *, stale: bool = False) -> str:
    stale_marker = " ~" if stale else ""
    return (
        f"{AI_ICON} {compact_count(status.input_tokens_per_minute)}↓/m "
        f"{compact_count(status.output_tokens_per_minute)}↑/m{stale_marker}"
    )


def render_error(message: str) -> str:
    return f"{AI_ICON} {message}"


def _management_key_file() -> Path:
    configured = os.environ.get("OPENROUTER_MANAGEMENT_KEY_FILE")
    if configured:
        return Path(configured).expanduser()
    return Path("~/.config/openrouter/management-key").expanduser()


def load_management_key() -> str:
    for env_name in ("OPENROUTER_MANAGEMENT_KEY", "OPENROUTER_API_KEY"):
        value = os.environ.get(env_name, "").strip()
        if value:
            return value
    try:
        return _management_key_file().read_text(encoding="utf-8").strip()
    except FileNotFoundError as exc:
        raise RuntimeError("management key missing") from exc


def _request_json(
    method: str,
    path: str,
    key: str,
    payload: dict[str, Any] | None = None,
) -> dict[str, Any]:
    body = None
    headers = {
        "Accept": "application/json",
        "Authorization": f"Bearer {key}",
        "User-Agent": "qtile-openrouter-status/6",
    }
    if payload is not None:
        body = json.dumps(payload, separators=(",", ":")).encode("utf-8")
        headers["Content-Type"] = "application/json"
    request = urllib.request.Request(
        f"{API_BASE}{path}", data=body, headers=headers, method=method
    )
    try:
        with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT_SECONDS) as response:
            return json.load(response)
    except urllib.error.HTTPError as exc:
        if exc.code in (401, 403):
            raise RuntimeError(
                "management key rejected (OpenRouter Analytics requires management-key scope)"
            ) from exc
        if exc.code == 429:
            raise RuntimeError("OpenRouter rate limited") from exc
        raise RuntimeError(f"OpenRouter HTTP {exc.code}") from exc
    except (urllib.error.URLError, TimeoutError) as exc:
        raise RuntimeError("OpenRouter unavailable") from exc


def _rows(payload: dict[str, Any]) -> list[dict[str, Any]]:
    metadata = payload.get("metadata") or payload.get("data", {}).get("metadata") or {}
    if metadata.get("truncated"):
        raise RuntimeError("OpenRouter analytics result truncated")
    rows = payload.get("data", {}).get("data", [])
    if not isinstance(rows, list):
        raise RuntimeError("OpenRouter analytics rows malformed")
    return [row for row in rows if isinstance(row, dict)]


def parse_token_totals(payload: dict[str, Any]) -> tuple[int, int]:
    prompt = 0.0
    completion = 0.0
    seen: set[tuple[Any, ...]] = set()
    for row in _rows(payload):
        bucket = (
            row.get("created_at__minute")
            or row.get("date__minute")
            or row.get("timestamp")
            or row.get("date")
        )
        try:
            row_prompt = float(row.get("tokens_prompt") or 0)
            row_completion = float(row.get("tokens_completion") or 0)
        except (TypeError, ValueError) as exc:
            raise RuntimeError("OpenRouter analytics token metric malformed") from exc
        signature = (bucket, row_prompt, row_completion)
        if signature in seen:
            continue
        seen.add(signature)
        prompt += row_prompt
        completion += row_completion
    return int(round(prompt)), int(round(completion))


def parse_usage_summary(payload: dict[str, Any]) -> tuple[int, float]:
    tokens = 0.0
    spend = 0.0
    for row in _rows(payload):
        try:
            row_tokens = row.get("tokens_total")
            if row_tokens is None:
                row_tokens = float(row.get("tokens_prompt") or 0) + float(
                    row.get("tokens_completion") or 0
                )
            tokens += float(row_tokens or 0)
            spend += float(row.get("total_usage") or 0)
        except (TypeError, ValueError) as exc:
            raise RuntimeError("OpenRouter usage metric malformed") from exc
    return int(round(tokens)), spend


def _parse_bucket_timestamp(row: dict[str, Any], granularity: str) -> datetime:
    value = (
        row.get(f"created_at__{granularity}")
        or row.get(f"date__{granularity}")
        or row.get("timestamp")
        or row.get("date")
    )
    if not value:
        raise RuntimeError("OpenRouter analytics bucket timestamp missing")
    try:
        parsed = datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except ValueError as exc:
        raise RuntimeError("OpenRouter analytics bucket timestamp malformed") from exc
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def parse_history_rows(payload: dict[str, Any], granularity: str) -> list[dict[str, Any]]:
    duration = {"minute": 60, "hour": 3600, "day": 86400}.get(granularity)
    if duration is None:
        raise ValueError(f"unsupported history granularity: {granularity}")
    result: list[dict[str, Any]] = []
    for row in _rows(payload):
        try:
            incoming = int(round(float(row.get("tokens_prompt") or 0)))
            outgoing = int(round(float(row.get("tokens_completion") or 0)))
            spend = float(row.get("total_usage") or 0)
        except (TypeError, ValueError) as exc:
            raise RuntimeError("OpenRouter history metric malformed") from exc
        result.append(
            {
                "start": _parse_bucket_timestamp(row, granularity),
                "seconds": duration,
                "input": incoming,
                "output": outgoing,
                "spend": spend,
            }
        )
    return result


def _closed_minute_window(now: datetime | None = None) -> tuple[datetime, datetime]:
    now = (now or datetime.now(timezone.utc)).astimezone(timezone.utc)
    end = now.replace(second=0, microsecond=0)
    return end - timedelta(seconds=RATE_WINDOW_SECONDS), end


def fetch_tokens(key: str, start: datetime, end: datetime) -> tuple[int, int]:
    payload = {
        "metrics": ["tokens_prompt", "tokens_completion"],
        "granularity": "minute",
        "time_range": {
            "start": start.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
            "end": end.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
        },
        "limit": 10,
    }
    return parse_token_totals(_request_json("POST", "/analytics/query", key, payload))


def fetch_usage_range(
    key: str, start: datetime, end: datetime, granularity: str = "hour"
) -> tuple[int, float]:
    payload = {
        "metrics": ["tokens_total", "total_usage"],
        "granularity": granularity,
        "time_range": {
            "start": start.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
            "end": end.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
        },
        "limit": 1000,
    }
    return parse_usage_summary(_request_json("POST", "/analytics/query", key, payload))


def fetch_history_range(
    key: str, start: datetime, end: datetime, granularity: str
) -> list[dict[str, Any]]:
    payload = {
        "metrics": ["tokens_prompt", "tokens_completion", "total_usage"],
        "granularity": granularity,
        "time_range": {
            "start": start.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
            "end": end.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
        },
        "limit": 1000,
    }
    response = _request_json("POST", "/analytics/query", key, payload)
    return parse_history_rows(response, granularity)


def fetch_balance(key: str) -> float:
    payload = _request_json("GET", "/credits", key)
    data = payload.get("data", {})
    try:
        total = float(data.get("total_credits") or 0)
        usage = float(data.get("total_usage") or 0)
    except (TypeError, ValueError) as exc:
        raise RuntimeError("OpenRouter credit payload malformed") from exc
    return max(total - usage, 0.0)


def _maximum_tpm() -> int:
    raw = os.environ.get("OPENROUTER_TPM_MAX", str(DEFAULT_MAX_TPM))
    try:
        return max(int(raw), 1)
    except ValueError:
        return DEFAULT_MAX_TPM


def validate_rate(input_tokens: int, output_tokens: int) -> None:
    if input_tokens < 0 or output_tokens < 0:
        raise RuntimeError("negative OpenRouter token rate")
    maximum = _maximum_tpm()
    if input_tokens + output_tokens > maximum:
        raise RuntimeError("implausible OpenRouter token rate")


def period_starts(now: datetime | None = None) -> dict[str, datetime]:
    if now is None:
        now = datetime.now().astimezone()
    elif now.tzinfo is None:
        now = now.astimezone()
    day = now.replace(hour=0, minute=0, second=0, microsecond=0)
    hour = now.replace(minute=0, second=0, microsecond=0)
    week = day - timedelta(days=day.weekday())
    month = day.replace(day=1)
    year = day.replace(month=1, day=1)
    return {"month": month, "week": week, "day": day, "hour": hour, "year": year}


def fetch_period_totals(key: str, now: datetime | None = None) -> dict[str, float | int]:
    if now is None:
        now = datetime.now().astimezone()
    elif now.tzinfo is None:
        now = now.astimezone()
    starts = period_starts(now)
    minute_start, minute_end = _closed_minute_window(now)
    windows: dict[str, tuple[datetime, datetime, str]] = {
        "minute": (minute_start, minute_end, "minute"),
        "hour": (starts["hour"], now, "hour"),
        "day": (starts["day"], now, "hour"),
        "week": (starts["week"], now, "hour"),
        "month": (starts["month"], now, "hour"),
        "year": (starts["year"], now, "hour"),
    }
    result: dict[str, float | int] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(windows)) as executor:
        futures = {
            period: executor.submit(fetch_usage_range, key, start, end, granularity)
            for period, (start, end, granularity) in windows.items()
        }
        for period, future in futures.items():
            tokens, spend = future.result()
            result[f"tokens_{period}"] = tokens
            if period not in ("hour", "minute"):
                result[f"spend_{period}"] = spend
    return result


def _cache_path() -> Path:
    root = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser()
    return root / "qtile" / "openrouter-status.json"


def _daemon_lock_path() -> Path:
    return _cache_path().with_suffix(".daemon.lock")


def _read_cache(path: Path) -> dict[str, Any] | None:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (FileNotFoundError, json.JSONDecodeError, OSError, TypeError):
        return None


def _status_from_cache(cache: dict[str, Any] | None) -> Status | None:
    if not cache:
        return None
    try:
        return Status(**cache["status"])
    except (KeyError, TypeError, ValueError):
        return None


def _write_document(path: Path, document: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    temporary.write_text(
        json.dumps(document, separators=(",", ":")),
        encoding="utf-8",
    )
    temporary.replace(path)


def _write_cache(
    path: Path,
    status: Status,
    *,
    fetched_at: float,
    metadata: dict[str, Any] | None = None,
) -> None:
    existing = _read_cache(path) or {}
    document: dict[str, Any] = {
        "fetched_at": fetched_at,
        "status": asdict(status),
    }
    for key in COLLECTOR_KEYS:
        if key in existing:
            document[key] = existing[key]
    if metadata:
        document.update(metadata)
    _write_document(path, document)


def _iso_now(now: datetime) -> str:
    return now.astimezone(timezone.utc).isoformat().replace("+00:00", "Z")


def _age_seconds(value: str | None, now: datetime) -> float:
    if not value:
        return float("inf")
    try:
        stamp = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError:
        return float("inf")
    return max((now.astimezone(timezone.utc) - stamp.astimezone(timezone.utc)).total_seconds(), 0)


def fetch_status(key: str, previous: Status | None = None) -> Status:
    now_utc = datetime.now(timezone.utc)
    start, end = _closed_minute_window(now_utc)
    window_end = end.isoformat().replace("+00:00", "Z")

    input_tokens = previous.input_tokens_per_minute if previous else 0
    output_tokens = previous.output_tokens_per_minute if previous else 0
    history_error = None
    if previous is None or previous.window_end != window_end:
        input_tokens, output_tokens = fetch_tokens(key, start, end)
        validate_rate(input_tokens, output_tokens)
        try:
            history.upsert_sample(
                start,
                RATE_WINDOW_SECONDS,
                input_tokens,
                output_tokens,
                source="live-minute",
            )
        except (OSError, sqlite3.Error) as exc:
            history_error = f"history storage unavailable: {exc}"

    values = asdict(previous) if previous else asdict(Status(input_tokens, output_tokens))
    values.update(
        input_tokens_per_minute=input_tokens,
        output_tokens_per_minute=output_tokens,
        window_end=window_end,
        last_error=history_error,
    )

    usage_due = (
        previous is None
        or _age_seconds(previous.usage_fetched_at, now_utc) >= USAGE_REFRESH_SECONDS
    )
    balance_due = (
        previous is None
        or _age_seconds(previous.balance_fetched_at, now_utc) >= BALANCE_REFRESH_SECONDS
    )
    if usage_due or balance_due:
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
            usage_future = executor.submit(fetch_period_totals, key) if usage_due else None
            balance_future = executor.submit(fetch_balance, key) if balance_due else None

            if usage_future is not None:
                try:
                    values.update(usage_future.result())
                    values["usage_fetched_at"] = _iso_now(now_utc)
                    values["usage_error"] = None
                except RuntimeError as exc:
                    values["usage_error"] = str(exc)

            if balance_future is not None:
                try:
                    values["balance_usd"] = balance_future.result()
                    values["balance_fetched_at"] = _iso_now(now_utc)
                    values["balance_error"] = None
                except RuntimeError as exc:
                    values["balance_error"] = str(exc)

    return Status(**values)


def status_with_cache(key: str, *, force: bool = False) -> tuple[Status, bool]:
    path = _cache_path()
    lock_path = path.with_suffix(".lock")
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    with lock_path.open("a+", encoding="utf-8") as lock:
        fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
        cache = _read_cache(path)
        cached_status = _status_from_cache(cache)
        fetched_at = float((cache or {}).get("fetched_at", 0))
        if not force and cached_status and time.time() - fetched_at < CACHE_TTL_SECONDS:
            return cached_status, False
        try:
            status = fetch_status(key, cached_status)
        except RuntimeError as exc:
            if cached_status:
                return replace(cached_status, last_error=str(exc)), True
            raise
        _write_cache(path, status, fetched_at=time.time())
        return status, False


def _persist_history_rows(rows: list[dict[str, Any]], source: str) -> int:
    count = 0
    for row in rows:
        history.upsert_sample(
            row["start"],
            int(row["seconds"]),
            int(row["input"]),
            int(row["output"]),
            spend=float(row["spend"]),
            source=source,
        )
        count += 1
    return count


def backfill_history(key: str, now: datetime | None = None) -> dict[str, int]:
    """Seed one year locally without pretending coarse history is minute data."""
    now = (now or datetime.now(timezone.utc)).astimezone(timezone.utc)
    hour_end = now.replace(minute=0, second=0, microsecond=0)
    fine_start = (hour_end - timedelta(days=BACKFILL_FINE_DAYS)).replace(
        hour=0, minute=0, second=0, microsecond=0
    )
    year_start = (hour_end - timedelta(days=BACKFILL_DAYS)).replace(
        hour=0, minute=0, second=0, microsecond=0
    )
    counts = {"day": 0, "hour": 0}
    if year_start < fine_start:
        counts["day"] = _persist_history_rows(
            fetch_history_range(key, year_start, fine_start, "day"),
            "backfill-day",
        )
    if fine_start < hour_end:
        counts["hour"] = _persist_history_rows(
            fetch_history_range(key, fine_start, hour_end, "hour"),
            "backfill-hour",
        )
    history.set_metadata("backfill_last_success", _iso_now(now))
    return counts


def _launch_backfill_if_due() -> None:
    try:
        due = history.claim_due("backfill_last_attempt", BACKFILL_INTERVAL_SECONDS)
    except (OSError, sqlite3.Error):
        return
    if not due:
        return
    try:
        subprocess.Popen(
            [sys.executable, str(Path(__file__).resolve()), "--backfill"],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
    except OSError:
        pass


def _history_diagnostics() -> dict[str, Any]:
    try:
        info = history.summary()
        info["backfill_last_success"] = history.get_metadata("backfill_last_success")
        info["history_error"] = None
        return info
    except (OSError, sqlite3.Error) as exc:
        return {
            "rows": 0,
            "oldest": None,
            "newest": None,
            "backfill_last_success": None,
            "history_error": str(exc),
        }


def _json_payload(status: Status, stale: bool) -> dict[str, Any]:
    payload = asdict(status)
    payload["total_tokens_per_minute"] = status.total_tokens_per_minute
    payload["stale"] = stale
    info = _history_diagnostics()
    payload["history_rows"] = info.get("rows", 0)
    payload["history_oldest"] = info.get("oldest")
    payload["history_newest"] = info.get("newest")
    payload["history_backfill"] = info.get("backfill_last_success")
    payload["history_error"] = info.get("history_error")
    return payload


def _pid_alive(pid: int | None) -> bool:
    if not pid or pid <= 1:
        return False
    try:
        os.kill(int(pid), 0)
    except (OSError, ValueError, TypeError):
        return False
    return True


def _cached_json_payload(
    cache: dict[str, Any] | None,
    *,
    now: float | None = None,
) -> dict[str, Any] | None:
    if not cache:
        return None
    now = time.time() if now is None else float(now)
    status = _status_from_cache(cache)
    collector_error = cache.get("collector_error")
    if status is None:
        if collector_error:
            return {
                "error": str(collector_error),
                "collector_pid": cache.get("collector_pid"),
                "collector_parent_pid": cache.get("collector_parent_pid"),
                "collector_heartbeat": cache.get("collector_heartbeat"),
            }
        return None

    payload = asdict(status)
    payload["total_tokens_per_minute"] = status.total_tokens_per_minute
    heartbeat = float(cache.get("collector_heartbeat") or 0)
    fetched_at = float(cache.get("fetched_at") or 0)
    if heartbeat:
        stale = now - heartbeat > DAEMON_HEARTBEAT_STALE_SECONDS
    else:
        stale = now - fetched_at > max(USAGE_REFRESH_SECONDS * 2, 90)
    stale = bool(cache.get("collector_stale")) or stale
    payload["stale"] = stale
    payload["collector_pid"] = cache.get("collector_pid")
    payload["collector_parent_pid"] = cache.get("collector_parent_pid")
    payload["collector_heartbeat"] = cache.get("collector_heartbeat")
    payload["collector_error"] = collector_error
    if collector_error and not payload.get("last_error"):
        payload["last_error"] = str(collector_error)
    return payload


def _collector_is_fresh(
    cache: dict[str, Any] | None,
    parent_pid: int,
    *,
    now: float | None = None,
) -> bool:
    if not cache:
        return False
    now = time.time() if now is None else float(now)
    try:
        pid = int(cache.get("collector_pid") or 0)
        recorded_parent = int(cache.get("collector_parent_pid") or 0)
        heartbeat = float(cache.get("collector_heartbeat") or 0)
    except (TypeError, ValueError):
        return False
    return (
        recorded_parent == int(parent_pid)
        and heartbeat > 0
        and now - heartbeat <= DAEMON_HEARTBEAT_STALE_SECONDS
        and _pid_alive(pid)
    )


def _ensure_daemon(parent_pid: int, *, now: float | None = None) -> bool:
    cache = _read_cache(_cache_path())
    if _collector_is_fresh(cache, parent_pid, now=now):
        return False
    try:
        subprocess.Popen(
            [
                sys.executable,
                str(Path(__file__).resolve()),
                "--daemon",
                "--parent-pid",
                str(int(parent_pid)),
            ],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
    except OSError:
        return False
    return True


def _collector_metadata(
    parent_pid: int,
    *,
    stale: bool,
    error: str | None,
) -> dict[str, Any]:
    return {
        "collector_pid": os.getpid(),
        "collector_parent_pid": int(parent_pid),
        "collector_heartbeat": time.time(),
        "collector_error": error,
        "collector_stale": bool(stale),
    }


def _publish_collector_error(parent_pid: int, error: str) -> None:
    path = _cache_path()
    cache = _read_cache(path) or {}
    status = _status_from_cache(cache)
    if status is not None:
        status = replace(status, last_error=error)
        _write_cache(
            path,
            status,
            fetched_at=float(cache.get("fetched_at") or 0),
            metadata=_collector_metadata(parent_pid, stale=True, error=error),
        )
        return
    document = {
        key: value
        for key, value in cache.items()
        if key not in COLLECTOR_KEYS
    }
    document.update(_collector_metadata(parent_pid, stale=True, error=error))
    _write_document(path, document)


def _sleep_while_parent_alive(parent_pid: int, seconds: float) -> bool:
    deadline = time.monotonic() + max(seconds, 0)
    while time.monotonic() < deadline:
        if not _pid_alive(parent_pid):
            return False
        time.sleep(min(0.1, max(deadline - time.monotonic(), 0)))
    return _pid_alive(parent_pid)


def run_daemon(parent_pid: int) -> int:
    """Run one Qtile-owned collector. Network work never occurs in widget polling."""
    lock_path = _daemon_lock_path()
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    with lock_path.open("a+", encoding="utf-8") as lock:
        try:
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return 0
        lock.seek(0)
        lock.truncate()
        lock.write(json.dumps({"pid": os.getpid(), "parent_pid": int(parent_pid)}))
        lock.flush()

        while _pid_alive(parent_pid):
            try:
                key = load_management_key()
                if not key:
                    raise RuntimeError("management key missing")
                status, stale = status_with_cache(key, force=True)
                _write_cache(
                    _cache_path(),
                    status,
                    fetched_at=time.time(),
                    metadata=_collector_metadata(parent_pid, stale=stale, error=None),
                )
                _launch_backfill_if_due()
            except (RuntimeError, OSError, sqlite3.Error) as exc:
                _publish_collector_error(parent_pid, str(exc))
            if not _sleep_while_parent_alive(parent_pid, DAEMON_POLL_SECONDS):
                break
    return 0


def _doctor_payload() -> dict[str, Any]:
    info = _history_diagnostics()
    info["management_key_file"] = str(_management_key_file())
    info["management_key_available"] = bool(
        os.environ.get("OPENROUTER_MANAGEMENT_KEY")
        or os.environ.get("OPENROUTER_API_KEY")
        or _management_key_file().exists()
    )
    cache = _read_cache(_cache_path()) or {}
    for key in COLLECTOR_KEYS:
        info[key] = cache.get(key)
    info["collector_alive"] = _pid_alive(cache.get("collector_pid"))
    return info


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", action="store_true", help="print cache-only machine-readable status")
    parser.add_argument("--force", action="store_true", help="perform a synchronous one-shot provider refresh")
    parser.add_argument("--daemon", action="store_true", help="run the Qtile-owned background collector")
    parser.add_argument("--parent-pid", type=int, help="owner PID for daemon lifecycle")
    parser.add_argument("--backfill", action="store_true", help="backfill persistent history and exit")
    parser.add_argument("--history", choices=tuple(history.TIMEFRAME_SECONDS), help="print a local history range and exit")
    parser.add_argument("--points", type=int, default=82, help="maximum local history points")
    parser.add_argument("--doctor", action="store_true", help="print local telemetry diagnostics and exit")
    args = parser.parse_args(argv)

    try:
        if args.history:
            print(json.dumps(history.query_series(args.history, points=args.points), sort_keys=True))
            return 0

        if args.doctor:
            print(json.dumps(_doctor_payload(), sort_keys=True))
            return 0

        if args.daemon:
            return run_daemon(args.parent_pid or os.getppid())

        if args.json and not args.force:
            parent_pid = args.parent_pid or os.getppid()
            _ensure_daemon(parent_pid)
            payload = _cached_json_payload(_read_cache(_cache_path()))
            if payload is None:
                payload = {"error": "starting", "collector_parent_pid": parent_pid}
            print(json.dumps(payload, sort_keys=True))
            return 0

        key = load_management_key()
        if not key:
            raise RuntimeError("management key missing")
        if args.backfill:
            print(json.dumps(backfill_history(key), sort_keys=True))
            return 0
        status, stale = status_with_cache(key, force=args.force)
    except (RuntimeError, OSError, sqlite3.Error) as exc:
        if args.json or args.history or args.backfill:
            print(json.dumps({"error": str(exc)}))
        else:
            short = "key?" if "key" in str(exc).lower() else "offline"
            print(render_error(short))
        return 1

    _launch_backfill_if_due()
    if args.json:
        print(json.dumps(_json_payload(status, stale), sort_keys=True))
    else:
        print(render(status, stale=stale))
    return 0


if __name__ == "__main__":
    sys.exit(main())
