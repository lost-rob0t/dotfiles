#!/usr/bin/env python3
"""Render trustworthy OpenRouter token-per-minute telemetry for the Qtile bar."""

from __future__ import annotations

import argparse
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
RED = "#dd546e"
IO = "#f6019d"
ACCENT = "#2de2e6"
CACHE_TTL_SECONDS = 4
RATE_WINDOW_SECONDS = 60
REQUEST_TIMEOUT_SECONDS = 5
DEFAULT_MAX_TPM = 10_000_000


@dataclass(frozen=True)
class Status:
    input_tokens_per_minute: int
    output_tokens_per_minute: int
    balance_usd: float | None = None
    window_end: str | None = None

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
        f'<span foreground="{IO}">{AI_ICON} '
        f"{compact_count(status.input_tokens_per_minute)}↓/m</span> "
        f'<span foreground="{ACCENT}">'
        f"{compact_count(status.output_tokens_per_minute)}↑/m{stale_marker}</span>"
    )


def render_error(message: str) -> str:
    return f'<span foreground="{RED}">{AI_ICON} {message}</span>'


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
        "User-Agent": "qtile-openrouter-status/3",
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


def parse_token_totals(payload: dict[str, Any]) -> tuple[int, int]:
    metadata = payload.get("metadata") or payload.get("data", {}).get("metadata") or {}
    if metadata.get("truncated"):
        raise RuntimeError("OpenRouter analytics result truncated")
    rows = payload.get("data", {}).get("data", [])
    if not isinstance(rows, list):
        raise RuntimeError("OpenRouter analytics rows malformed")

    prompt = 0.0
    completion = 0.0
    seen: set[tuple[Any, ...]] = set()
    for row in rows:
        if not isinstance(row, dict):
            continue
        bucket = (
            row.get("created_at__minute")
            or row.get("date__minute")
            or row.get("timestamp")
            or row.get("date")
        )
        row_prompt = float(row.get("tokens_prompt") or 0)
        row_completion = float(row.get("tokens_completion") or 0)
        signature = (bucket, row_prompt, row_completion)
        if signature in seen:
            continue
        seen.add(signature)
        prompt += row_prompt
        completion += row_completion
    return int(round(prompt)), int(round(completion))


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


def fetch_balance(key: str) -> float:
    payload = _request_json("GET", "/credits", key)
    data = payload.get("data", {})
    total = float(data.get("total_credits") or 0)
    usage = float(data.get("total_usage") or 0)
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


def fetch_status(key: str, previous: Status | None = None) -> Status:
    start, end = _closed_minute_window()
    input_tokens, output_tokens = fetch_tokens(key, start, end)
    validate_rate(input_tokens, output_tokens)
    balance = previous.balance_usd if previous else None
    try:
        balance = fetch_balance(key)
    except RuntimeError:
        pass
    return Status(
        input_tokens_per_minute=input_tokens,
        output_tokens_per_minute=output_tokens,
        balance_usd=balance,
        window_end=end.isoformat().replace("+00:00", "Z"),
    )


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
    parser.add_argument("--force", action="store_true", help="ignore the short poll cache")
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
