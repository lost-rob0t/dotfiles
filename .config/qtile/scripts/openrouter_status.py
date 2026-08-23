#!/usr/bin/env python3
"""Collect trustworthy OpenRouter rate, balance, token, and spend telemetry."""

from __future__ import annotations

import argparse
import concurrent.futures
import fcntl
import json
import os
import sys
import time
import urllib.error
import urllib.request
from dataclasses import asdict, dataclass
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any

API_BASE = "https://openrouter.ai/api/v1"
AI_ICON = "\U000F09D1"

CACHE_TTL_SECONDS = 0.75
RATE_WINDOW_SECONDS = 60
USAGE_REFRESH_SECONDS = 30
BALANCE_REFRESH_SECONDS = 30
REQUEST_TIMEOUT_SECONDS = 5
DEFAULT_MAX_TPM = 10_000_000


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
    spend_month: float | None = None
    spend_week: float | None = None
    spend_day: float | None = None
    usage_fetched_at: str | None = None
    balance_fetched_at: str | None = None

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
        "User-Agent": "qtile-openrouter-status/4",
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
            raise RuntimeError("management key rejected") from exc
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


def fetch_usage_range(key: str, start: datetime, end: datetime) -> tuple[int, float]:
    payload = {
        "metrics": ["tokens_total", "total_usage"],
        "granularity": "hour",
        "time_range": {
            "start": start.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
            "end": end.astimezone(timezone.utc).isoformat().replace("+00:00", "Z"),
        },
        "limit": 1000,
    }
    return parse_usage_summary(_request_json("POST", "/analytics/query", key, payload))


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
    return {"month": month, "week": week, "day": day, "hour": hour}


def fetch_period_totals(key: str, now: datetime | None = None) -> dict[str, float | int]:
    if now is None:
        now = datetime.now().astimezone()
    elif now.tzinfo is None:
        now = now.astimezone()
    starts = period_starts(now)
    periods = ("month", "week", "day", "hour")
    result: dict[str, float | int] = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(periods)) as executor:
        futures = {
            period: executor.submit(fetch_usage_range, key, starts[period], now)
            for period in periods
        }
        for period in periods:
            tokens, spend = futures[period].result()
            result[f"tokens_{period}"] = tokens
            if period != "hour":
                result[f"spend_{period}"] = spend
    return result


def _cache_path() -> Path:
    root = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser()
    return root / "qtile" / "openrouter-status.json"


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


def _write_cache(path: Path, status: Status, *, fetched_at: float) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(".tmp")
    temporary.write_text(
        json.dumps(
            {"fetched_at": fetched_at, "status": asdict(status)},
            separators=(",", ":"),
        ),
        encoding="utf-8",
    )
    temporary.replace(path)


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
    if previous is None or previous.window_end != window_end:
        input_tokens, output_tokens = fetch_tokens(key, start, end)
        validate_rate(input_tokens, output_tokens)

    values = asdict(previous) if previous else asdict(Status(input_tokens, output_tokens))
    values.update(
        input_tokens_per_minute=input_tokens,
        output_tokens_per_minute=output_tokens,
        window_end=window_end,
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
                except RuntimeError:
                    pass

            if balance_future is not None:
                try:
                    values["balance_usd"] = balance_future.result()
                    values["balance_fetched_at"] = _iso_now(now_utc)
                except RuntimeError:
                    pass

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
        except RuntimeError:
            if cached_status:
                return cached_status, True
            raise
        _write_cache(path, status, fetched_at=time.time())
        return status, False


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", action="store_true", help="print machine-readable status")
    parser.add_argument("--force", action="store_true", help="ignore the short process cache")
    args = parser.parse_args(argv)
    try:
        key = load_management_key()
        if not key:
            raise RuntimeError("management key missing")
        status, stale = status_with_cache(key, force=args.force)
    except RuntimeError as exc:
        if args.json:
            print(json.dumps({"error": str(exc)}))
        else:
            short = "key?" if "key" in str(exc).lower() else "offline"
            print(render_error(short))
        return 1
    if args.json:
        payload = asdict(status)
        payload["total_tokens_per_minute"] = status.total_tokens_per_minute
        payload["stale"] = stale
        print(json.dumps(payload, sort_keys=True))
    else:
        print(render(status, stale=stale))
    return 0


if __name__ == "__main__":
    sys.exit(main())
