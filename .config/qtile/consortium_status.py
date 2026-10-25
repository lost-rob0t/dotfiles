"""Bounded local Consortium status projection reader for Qtile."""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import json
import os
import threading
import time

MAX_BYTES = 65536
MAX_RUNS = 16
STATES = {"running", "completed", "failed", "cancelled"}


def clean(value, *, limit=96):
    if value is None:
        return None
    if not isinstance(value, str):
        raise ValueError("status strings must be strings or null")
    value = "".join(ch for ch in value if ch.isprintable()).strip()
    return value[:limit] or None


@dataclass(frozen=True)
class RunStatus:
    run_id: str
    sequence: int
    state: str
    phase: str | None
    worker: str | None
    event_type: str | None
    updated_at: str | None


@dataclass(frozen=True)
class Snapshot:
    active_count: int
    runs: tuple[RunStatus, ...]
    fetched_at: float
    source_mtime: float


def decode_status(payload, *, fetched_at: float, source_mtime: float) -> Snapshot:
    if not isinstance(payload, dict) or type(payload.get("schema_version")) is not int or payload["schema_version"] != 1:
        raise ValueError("unsupported Consortium status schema")
    rows = payload.get("runs")
    active_count = payload.get("active_count")
    if not isinstance(rows, list) or len(rows) > MAX_RUNS:
        raise ValueError("invalid Consortium run list")
    if type(active_count) is not int or not 0 <= active_count <= MAX_RUNS:
        raise ValueError("invalid Consortium active count")

    runs = []
    seen = set()
    for row in rows:
        if not isinstance(row, dict):
            raise ValueError("invalid Consortium run")
        run_id = clean(row.get("run_id"), limit=128)
        sequence = row.get("sequence")
        state = row.get("state")
        if not run_id or run_id in seen or type(sequence) is not int or sequence < 1 or state not in STATES:
            raise ValueError("invalid Consortium run identity")
        seen.add(run_id)
        phase = clean(row.get("phase"))
        worker = clean(row.get("worker"))
        event_type = clean(row.get("event_type"))
        updated_at = clean(row.get("updated_at"), limit=64)
        if phase == "none":
            phase = None
        if worker == "none":
            worker = None
        runs.append(RunStatus(run_id, sequence, state, phase, worker, event_type, updated_at))

    computed_active = sum(run.state == "running" for run in runs)
    if computed_active != active_count:
        raise ValueError("Consortium active count disagrees with runs")
    return Snapshot(active_count, tuple(runs), fetched_at, source_mtime)


def default_status_path() -> Path:
    override = os.environ.get("CONSORTIUM_QTILE_STATUS_FILE")
    if override:
        return Path(override).expanduser()
    state_home = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state"))
    return state_home / "consortium" / "qtile-status.json"


def latest_active(snapshot: Snapshot | None) -> RunStatus | None:
    if snapshot is None:
        return None
    return next((run for run in snapshot.runs if run.state == "running"), None)


def latest_run(snapshot: Snapshot | None) -> RunStatus | None:
    return snapshot.runs[0] if snapshot and snapshot.runs else None


def render_runs(snapshot: Snapshot | None, failed=False) -> tuple[str, str]:
    if snapshot is None:
        return "CONS --", "neutral"
    if failed:
        return f"CONS ~{snapshot.active_count}", "neutral"
    if snapshot.active_count:
        return f"CONS {snapshot.active_count}", "active"
    run = latest_run(snapshot)
    if run is None:
        return "CONS idle", "neutral"
    labels = {"completed": "✓", "failed": "!", "cancelled": "×"}
    levels = {"completed": "ok", "failed": "error", "cancelled": "warn"}
    return f"CONS {labels.get(run.state, 'idle')}", levels.get(run.state, "neutral")


def render_phase(snapshot: Snapshot | None, failed=False) -> tuple[str, str]:
    if snapshot is None:
        return "ADARD --", "neutral"
    run = latest_active(snapshot) or latest_run(snapshot)
    if run is None:
        return "ADARD idle", "neutral"
    if failed:
        return f"ADARD ~{run.phase or run.state}", "neutral"
    if run.state == "running":
        return f"ADARD {run.phase or 'running'}", "active"
    levels = {"completed": "ok", "failed": "error", "cancelled": "warn"}
    return f"ADARD {run.state}", levels.get(run.state, "neutral")


def details(snapshot: Snapshot | None, failed=False, path: Path | None = None) -> str:
    if snapshot is None:
        return f"status: unavailable\nsource: {path or default_status_path()}"
    age = max(0.0, time.time() - snapshot.source_mtime)
    lines = [
        f"source: {path or default_status_path()}",
        f"collector: {'invalid; showing last good snapshot' if failed else 'ok'}",
        f"active runs: {snapshot.active_count}",
        f"file age: {age:.0f}s",
    ]
    for run in snapshot.runs[:5]:
        fields = [run.state, run.phase or "-", run.worker or "-", f"seq={run.sequence}"]
        lines.append(f"{run.run_id}: " + " / ".join(fields))
    return "\n".join(lines)


class ConsortiumFeed:
    """Shared bounded file poller; Qtile render code only reads memory."""

    def __init__(self, path: Path | str | None = None, interval: float = 2.0):
        if not 0.5 <= interval <= 60:
            raise ValueError("invalid Consortium polling interval")
        self.path = Path(path) if path is not None else default_status_path()
        self.interval = interval
        self._lock = threading.Lock()
        self._wake = threading.Event()
        self._thread = None
        self._users = 0
        self._snapshot = None
        self._failed = False
        self._mtime_ns = None

    def read(self) -> tuple[Snapshot | None, bool]:
        with self._lock:
            return self._snapshot, self._failed

    def refresh_once(self) -> None:
        try:
            stat = self.path.stat()
            if stat.st_size > MAX_BYTES:
                raise ValueError("Consortium status file too large")
            with self._lock:
                if self._snapshot is not None and self._mtime_ns == stat.st_mtime_ns:
                    return
            raw = self.path.read_bytes()
            if len(raw) > MAX_BYTES:
                raise ValueError("Consortium status file too large")
            payload = json.loads(raw.decode("utf-8"))
            snapshot = decode_status(payload, fetched_at=time.time(), source_mtime=stat.st_mtime)
            with self._lock:
                self._snapshot = snapshot
                self._failed = False
                self._mtime_ns = stat.st_mtime_ns
        except FileNotFoundError:
            with self._lock:
                self._snapshot = None
                self._failed = False
                self._mtime_ns = None
        except (OSError, UnicodeError, ValueError, TypeError, json.JSONDecodeError):
            with self._lock:
                self._failed = True

    def acquire(self) -> None:
        with self._lock:
            self._users += 1
            if self._thread is None:
                self._thread = threading.Thread(target=self._run, name="qtile-consortium-status", daemon=True)
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


_feeds: dict[str, ConsortiumFeed] = {}


def shared_feed(path: Path | str | None = None) -> ConsortiumFeed:
    resolved = str(Path(path) if path is not None else default_status_path())
    if resolved not in _feeds:
        if len(_feeds) >= 8:
            raise ValueError("too many Consortium status feeds")
        _feeds[resolved] = ConsortiumFeed(resolved)
    return _feeds[resolved]
