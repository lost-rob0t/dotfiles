"""Bounded localhost quota snapshots; no provider credentials or token guesses."""
from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
import ipaddress
import json
import math
import threading
import time
from urllib.parse import urlsplit
from urllib.request import HTTPRedirectHandler, ProxyHandler, Request, build_opener

MAX_BYTES = 262144
LABELS = {"zai": "z.AI", "gpt": "GPT"}
STATES = {"ok", "unknown", "unavailable", "disabled", "stale", "expired"}


def finite(value, low=0, high=1e15):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        return None
    return float(value) if low <= value <= high and math.isfinite(value) else None


def clean(value, fallback="unknown"):
    if not isinstance(value, str) or not value:
        return fallback
    return "".join(c for c in value if c.isprintable())[:64]


@dataclass(frozen=True)
class Window:
    id: str
    meter: str
    label: str
    used_percent: float | None
    state: str
    resets_at: float | None


@dataclass(frozen=True)
class Provider:
    id: str
    state: str
    plan: str
    source: str
    scope: str
    updated_at: float
    windows: tuple[Window, ...]


@dataclass(frozen=True)
class Snapshot:
    generated_at: float
    fetched_at: float
    stale_after: float
    providers: tuple[Provider, ...]


def decode_quotas(payload, now: float) -> Snapshot:
    if not isinstance(payload, dict) or type(payload.get("schema_version")) is not int or payload["schema_version"] != 1:
        raise ValueError("unsupported quota schema")
    rows = payload.get("providers")
    generated = finite(payload.get("generated_at"), low=1)
    ttl = finite(payload.get("stale_after_seconds"), low=1, high=10800)
    if not isinstance(rows, list) or len(rows) > 8 or generated is None or ttl is None:
        raise ValueError("invalid quota snapshot")
    providers = []
    seen = set()
    for row in rows:
        if not isinstance(row, dict):
            raise ValueError("invalid provider")
        key = row.get("id")
        if key not in LABELS:
            continue
        if key in seen:
            raise ValueError("duplicate provider")
        seen.add(key)
        raw_windows = row.get("windows")
        if not isinstance(raw_windows, list) or len(raw_windows) > 64:
            raise ValueError("invalid quota windows")
        windows, ids = [], set()
        for raw in raw_windows:
            if not isinstance(raw, dict):
                raise ValueError("invalid quota window")
            identity = clean(raw.get("id"))
            if identity in ids:
                raise ValueError("duplicate quota window")
            ids.add(identity)
            state = raw.get("state")
            state = state if state in STATES else "unknown"
            percent = finite(raw.get("used_percent"), high=100)
            if percent is None and state == "ok":
                state = "unknown"
            windows.append(Window(identity, clean(raw.get("meter")), clean(raw.get("label"), "period?"),
                                  percent, state, finite(raw.get("resets_at"), low=1)))
        state = row.get("state")
        providers.append(Provider(key, state if state in STATES else "unknown",
                                  clean(row.get("plan")), clean(row.get("source")), clean(row.get("scope")),
                                  finite(row.get("updated_at")) or 0, tuple(windows)))
    return Snapshot(generated, now, ttl, tuple(providers))


def find_provider(snapshot: Snapshot | None, key: str) -> Provider | None:
    return next((p for p in snapshot.providers if p.id == key), None) if snapshot else None


def render_quota(snapshot: Snapshot | None, key: str, index: int, now: float, *,
                 offline=False, warning=70, critical=90) -> tuple[str, str]:
    if key not in LABELS or not 0 <= warning < critical <= 100:
        raise ValueError("invalid quota display configuration")
    item = find_provider(snapshot, key)
    if item is None or not item.windows:
        return f"{LABELS[key]} {item.state if item and item.state != 'ok' else 'unavailable'}", "neutral"
    assert snapshot is not None
    row = item.windows[index % len(item.windows)]
    state = row.state if row.state != "ok" else item.state
    if row.resets_at is not None and now >= row.resets_at:
        state = "expired"
    elif offline or not item.updated_at or any(now - stamp > snapshot.stale_after or stamp > now + 30
                                               for stamp in (item.updated_at, snapshot.generated_at, snapshot.fetched_at)):
        state = "stale"
    prefix = f"{LABELS[key]} {row.label}"
    if key == "gpt":
        # Even the compact bar preserves which account meter this really is.
        prefix = f"{LABELS[key]} {row.meter} {row.label}"
    if state != "ok" or row.used_percent is None:
        return f"{prefix} ? {state}", "neutral"
    percent = row.used_percent
    level = "red" if percent >= critical else "yellow" if percent >= warning else "green"
    return f"{prefix} {percent:g}%", level


def details(snapshot: Snapshot | None, key: str, now: float, *, offline=False) -> str:
    item = find_provider(snapshot, key)
    if item is None:
        return "Quota unavailable: enable llm-log provider telemetry and check its local API."
    lines = [f"plan: {item.plan}", f"source: {item.source}", f"scope: {item.scope}"]
    if key == "gpt":
        lines.append("Account meters (e.g. Codex), not all ChatGPT web/voice usage.")
    for i, row in enumerate(item.windows):
        lines.append(render_quota(snapshot, key, i, now, offline=offline)[0])
        if row.resets_at is not None:
            try:
                stamp = datetime.fromtimestamp(row.resets_at, timezone.utc).isoformat()
                lines.append(f"  reset: {stamp}")
            except (ValueError, OverflowError, OSError):
                lines.append("  reset: unavailable")
    return "\n".join(lines)


def validate_base_url(value: str) -> str:
    parsed = urlsplit(value)
    try:
        host = parsed.hostname
        local = host == "localhost" or ipaddress.ip_address(host or "").is_loopback
        port = parsed.port
    except ValueError:
        raise ValueError("llm-log URL must use a loopback host") from None
    if (not local or parsed.scheme not in ("http", "https") or parsed.username is not None
            or parsed.password is not None or parsed.query or parsed.fragment
            or parsed.path not in ("", "/") or (port is not None and port == 0)):
        raise ValueError("llm-log URL must be a credential-free loopback origin")
    return value.rstrip("/")


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, *_args, **_kwargs):
        raise ValueError("llm-log redirects are disabled")


def fetch_json(url: str):
    opener = build_opener(ProxyHandler({}), NoRedirect())
    with opener.open(Request(url, headers={"Accept": "application/json"}), timeout=3) as response:
        raw = response.read(MAX_BYTES + 1)
    if len(raw) > MAX_BYTES:
        raise ValueError("llm-log response too large")
    return json.loads(raw)


class QuotaFeed:
    """One bounded daemon owner shared across the quota pair and all screens."""
    def __init__(self, base_url: str, interval: float = 30):
        if not 5 <= interval <= 3600:
            raise ValueError("invalid local polling interval")
        self.base_url = validate_base_url(base_url)
        self.interval = interval
        self._lock = threading.Lock()
        self._wake = threading.Event()
        self._thread = None
        self._users = 0
        self._snapshot = None
        self._failed = False

    def read(self) -> tuple[Snapshot | None, bool]:
        with self._lock:
            return self._snapshot, self._failed

    def publish(self, snapshot: Snapshot) -> None:
        with self._lock:
            self._snapshot, self._failed = snapshot, False

    def refresh_once(self) -> None:
        try:
            self.publish(decode_quotas(fetch_json(self.base_url + "/api/v1/quotas"), time.time()))
        except (OSError, ValueError, TypeError, KeyError):
            # Never persist or show a server error body/exception that might contain secrets.
            with self._lock:
                self._failed = True

    def acquire(self) -> None:
        with self._lock:
            self._users += 1
            if self._thread is None:
                self._thread = threading.Thread(target=self._run, name="qtile-llm-log-quotas", daemon=True)
                self._thread.start()
        self._wake.set()

    def release(self) -> None:
        with self._lock:
            self._users = max(0, self._users - 1)
        self._wake.set()

    def _run(self) -> None:
        while True:
            with self._lock:
                active = self._users > 0
            if active:
                self.refresh_once()
            self._wake.wait(self.interval if active else None)
            self._wake.clear()


_feeds: dict[str, QuotaFeed] = {}


def shared_feed(base_url: str) -> QuotaFeed:
    # Called only from Qtile configuration, never worker threads.
    url = validate_base_url(base_url)
    if url not in _feeds:
        if len(_feeds) >= 8:
            raise ValueError("too many llm-log endpoints")
        _feeds[url] = QuotaFeed(url)
    return _feeds[url]
