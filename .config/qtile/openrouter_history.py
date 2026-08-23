#!/usr/bin/env python3
"""Persistent, local OpenRouter telemetry history and graph aggregation."""

from __future__ import annotations

import math
import os
import sqlite3
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable

TIMEFRAMES: tuple[tuple[str, int], ...] = (
    ("1m", 60),
    ("5m", 5 * 60),
    ("1h", 60 * 60),
    ("6h", 6 * 60 * 60),
    ("12h", 12 * 60 * 60),
    ("1d", 24 * 60 * 60),
    ("1w", 7 * 24 * 60 * 60),
    ("1mo", 30 * 24 * 60 * 60),
    ("1y", 365 * 24 * 60 * 60),
)
TIMEFRAME_SECONDS = dict(TIMEFRAMES)


def history_path() -> Path:
    configured = os.environ.get("OPENROUTER_HISTORY_DB")
    if configured:
        return Path(configured).expanduser()
    root = Path(os.environ.get("XDG_STATE_HOME", "~/.local/state")).expanduser()
    return root / "qtile" / "openrouter-history.sqlite3"


def _connect(path: Path | None = None) -> sqlite3.Connection:
    path = path or history_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    connection = sqlite3.connect(path, timeout=2.0)
    connection.row_factory = sqlite3.Row
    connection.execute("PRAGMA journal_mode=WAL")
    connection.execute("PRAGMA busy_timeout=2000")
    connection.executescript(
        """
        CREATE TABLE IF NOT EXISTS usage_samples (
            bucket_start INTEGER NOT NULL,
            bucket_seconds INTEGER NOT NULL CHECK(bucket_seconds > 0),
            input_tokens INTEGER NOT NULL CHECK(input_tokens >= 0),
            output_tokens INTEGER NOT NULL CHECK(output_tokens >= 0),
            spend REAL,
            source TEXT NOT NULL,
            PRIMARY KEY (bucket_start, bucket_seconds)
        );
        CREATE INDEX IF NOT EXISTS usage_samples_window
            ON usage_samples(bucket_start, bucket_seconds);
        CREATE TABLE IF NOT EXISTS metadata (
            key TEXT PRIMARY KEY,
            value TEXT NOT NULL
        );
        """
    )
    return connection


def _epoch(value: datetime | int | float) -> int:
    if isinstance(value, datetime):
        if value.tzinfo is None:
            value = value.replace(tzinfo=timezone.utc)
        return int(value.timestamp())
    return int(value)


def upsert_sample(
    bucket_start: datetime | int | float,
    bucket_seconds: int,
    input_tokens: int,
    output_tokens: int,
    *,
    spend: float | None = None,
    source: str = "live",
    path: Path | None = None,
) -> None:
    """Idempotently persist one truthful provider bucket."""
    start = _epoch(bucket_start)
    duration = max(int(bucket_seconds), 1)
    incoming = max(int(input_tokens), 0)
    outgoing = max(int(output_tokens), 0)
    with _connect(path) as connection:
        connection.execute(
            """
            INSERT INTO usage_samples
                (bucket_start, bucket_seconds, input_tokens, output_tokens, spend, source)
            VALUES (?, ?, ?, ?, ?, ?)
            ON CONFLICT(bucket_start, bucket_seconds) DO UPDATE SET
                input_tokens=excluded.input_tokens,
                output_tokens=excluded.output_tokens,
                spend=COALESCE(excluded.spend, usage_samples.spend),
                source=excluded.source
            """,
            (start, duration, incoming, outgoing, spend, source),
        )


def set_metadata(key: str, value: str, *, path: Path | None = None) -> None:
    with _connect(path) as connection:
        connection.execute(
            "INSERT INTO metadata(key, value) VALUES (?, ?) "
            "ON CONFLICT(key) DO UPDATE SET value=excluded.value",
            (key, value),
        )


def get_metadata(key: str, *, path: Path | None = None) -> str | None:
    with _connect(path) as connection:
        row = connection.execute("SELECT value FROM metadata WHERE key=?", (key,)).fetchone()
    return None if row is None else str(row["value"])


def claim_due(key: str, interval_seconds: float, *, path: Path | None = None) -> bool:
    """Atomically claim periodic work so 1 Hz collectors do not fan it out."""
    now = time.time()
    with _connect(path) as connection:
        connection.execute("BEGIN IMMEDIATE")
        row = connection.execute("SELECT value FROM metadata WHERE key=?", (key,)).fetchone()
        try:
            last = float(row["value"]) if row is not None else 0.0
        except (TypeError, ValueError):
            last = 0.0
        if now - last < interval_seconds:
            connection.rollback()
            return False
        connection.execute(
            "INSERT INTO metadata(key, value) VALUES (?, ?) "
            "ON CONFLICT(key) DO UPDATE SET value=excluded.value",
            (key, str(now)),
        )
        connection.commit()
    return True


def _aligned_end(now: datetime | int | float | None = None) -> int:
    stamp = _epoch(now or datetime.now(timezone.utc))
    return stamp - stamp % 60


def _overlaps(start: int, end: int, accepted: Iterable[tuple[int, int]]) -> bool:
    return any(start < other_end and end > other_start for other_start, other_end in accepted)


def _window_rows(
    label: str,
    *,
    now: datetime | int | float | None = None,
    path: Path | None = None,
) -> tuple[int, int, list[sqlite3.Row]]:
    duration = TIMEFRAME_SECONDS[label]
    end = _aligned_end(now)
    start = end - duration
    with _connect(path) as connection:
        rows = connection.execute(
            """
            SELECT bucket_start, bucket_seconds, input_tokens, output_tokens, spend, source
              FROM usage_samples
             WHERE bucket_start >= ?
               AND bucket_start + bucket_seconds <= ?
             ORDER BY bucket_seconds DESC, bucket_start ASC
            """,
            (start, end),
        ).fetchall()

    # Coarser complete provider buckets win where granularities overlap. This
    # prevents a later hourly backfill from double-counting live minute rows.
    accepted_intervals: list[tuple[int, int]] = []
    accepted_rows: list[sqlite3.Row] = []
    for row in rows:
        row_start = int(row["bucket_start"])
        row_end = row_start + int(row["bucket_seconds"])
        if _overlaps(row_start, row_end, accepted_intervals):
            continue
        accepted_intervals.append((row_start, row_end))
        accepted_rows.append(row)
    accepted_rows.sort(key=lambda row: int(row["bucket_start"]))
    return start, end, accepted_rows


def query_series(
    label: str,
    *,
    points: int = 82,
    now: datetime | int | float | None = None,
    path: Path | None = None,
) -> dict[str, Any]:
    """Return truthful average token-rate samples for one display range.

    Provider buckets are never interpolated. If the history is coarse, the
    result intentionally contains fewer points rather than inventing detail.
    """
    if label not in TIMEFRAME_SECONDS:
        raise ValueError(f"unknown OpenRouter history range: {label}")
    point_limit = max(int(points), 1)
    start, end, rows = _window_rows(label, now=now, path=path)
    samples = [
        {
            "timestamp": int(row["bucket_start"]) + int(row["bucket_seconds"]) / 2.0,
            "input": float(row["input_tokens"]) * 60.0 / int(row["bucket_seconds"]),
            "output": float(row["output_tokens"]) * 60.0 / int(row["bucket_seconds"]),
            "bucket_seconds": int(row["bucket_seconds"]),
            "source": str(row["source"]),
        }
        for row in rows
    ]

    if len(samples) > point_limit:
        chunk_size = int(math.ceil(len(samples) / point_limit))
        reduced: list[dict[str, Any]] = []
        for offset in range(0, len(samples), chunk_size):
            chunk = samples[offset : offset + chunk_size]
            weights = [max(int(item["bucket_seconds"]), 1) for item in chunk]
            total_weight = float(sum(weights))
            reduced.append(
                {
                    "timestamp": sum(float(item["timestamp"]) * weight for item, weight in zip(chunk, weights)) / total_weight,
                    "input": sum(float(item["input"]) * weight for item, weight in zip(chunk, weights)) / total_weight,
                    "output": sum(float(item["output"]) * weight for item, weight in zip(chunk, weights)) / total_weight,
                    "bucket_seconds": sum(weights),
                    "source": "aggregate",
                }
            )
        samples = reduced

    ceiling = max(
        [float(item["input"]) for item in samples]
        + [float(item["output"]) for item in samples]
        + [0.0]
    )
    return {
        "range": label,
        "start": start,
        "end": end,
        "ceiling": ceiling,
        "samples": samples,
    }


def summary(*, path: Path | None = None) -> dict[str, Any]:
    with _connect(path) as connection:
        row = connection.execute(
            "SELECT COUNT(*) AS rows, MIN(bucket_start) AS oldest, "
            "MAX(bucket_start + bucket_seconds) AS newest FROM usage_samples"
        ).fetchone()
    return {
        "path": str(path or history_path()),
        "rows": int(row["rows"] or 0),
        "oldest": row["oldest"],
        "newest": row["newest"],
    }
