#!/usr/bin/env python3
"""Tests for persistent OpenRouter telemetry history."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from datetime import datetime, timezone
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[1] / "openrouter_history.py"
SPEC = importlib.util.spec_from_file_location("openrouter_history", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class OpenRouterHistoryTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.db = Path(self.temp.name) / "history.sqlite3"

    def tearDown(self):
        self.temp.cleanup()

    def test_requested_ranges_exist_in_order(self):
        self.assertEqual(
            tuple(label for label, _seconds in MODULE.TIMEFRAMES),
            ("1m", "5m", "1h", "6h", "12h", "1d", "1w", "1mo", "1y"),
        )

    def test_upsert_is_idempotent_and_persistent(self):
        start = datetime(2026, 8, 23, 4, 10, tzinfo=timezone.utc)
        MODULE.upsert_sample(start, 60, 100, 10, source="live", path=self.db)
        MODULE.upsert_sample(start, 60, 120, 12, source="live", path=self.db)
        info = MODULE.summary(path=self.db)
        self.assertEqual(info["rows"], 1)
        series = MODULE.query_series(
            "1m",
            now=datetime(2026, 8, 23, 4, 11, 20, tzinfo=timezone.utc),
            path=self.db,
        )
        self.assertEqual(series["samples"][0]["input"], 120.0)
        self.assertEqual(series["samples"][0]["output"], 12.0)

    def test_query_aligns_to_last_complete_minute(self):
        MODULE.upsert_sample(
            datetime(2026, 8, 23, 4, 10, tzinfo=timezone.utc),
            60,
            60,
            6,
            path=self.db,
        )
        series = MODULE.query_series(
            "1m",
            now=datetime(2026, 8, 23, 4, 11, 59, tzinfo=timezone.utc),
            path=self.db,
        )
        self.assertEqual(series["start"], int(datetime(2026, 8, 23, 4, 10, tzinfo=timezone.utc).timestamp()))
        self.assertEqual(len(series["samples"]), 1)

    def test_hour_backfill_prevents_minute_double_count(self):
        hour = datetime(2026, 8, 23, 3, 0, tzinfo=timezone.utc)
        MODULE.upsert_sample(hour, 3600, 3600, 360, source="backfill-hour", path=self.db)
        MODULE.upsert_sample(
            datetime(2026, 8, 23, 3, 30, tzinfo=timezone.utc),
            60,
            5000,
            500,
            source="live",
            path=self.db,
        )
        series = MODULE.query_series(
            "6h",
            now=datetime(2026, 8, 23, 4, 0, tzinfo=timezone.utc),
            path=self.db,
        )
        self.assertEqual(len(series["samples"]), 1)
        self.assertEqual(series["samples"][0]["source"], "backfill-hour")
        self.assertEqual(series["samples"][0]["input"], 60.0)

    def test_shared_ceiling_uses_both_io_series(self):
        end = datetime(2026, 8, 23, 4, 12, tzinfo=timezone.utc)
        MODULE.upsert_sample(
            datetime(2026, 8, 23, 4, 10, tzinfo=timezone.utc),
            60,
            900,
            90,
            path=self.db,
        )
        MODULE.upsert_sample(
            datetime(2026, 8, 23, 4, 11, tzinfo=timezone.utc),
            60,
            100,
            700,
            path=self.db,
        )
        series = MODULE.query_series("5m", now=end, path=self.db)
        self.assertEqual(series["ceiling"], 900.0)

    def test_coarse_history_is_not_interpolated(self):
        start = datetime(2026, 8, 20, 0, 0, tzinfo=timezone.utc)
        for day in range(3):
            MODULE.upsert_sample(
                int(start.timestamp()) + day * 86400,
                86400,
                86400,
                8640,
                source="backfill-day",
                path=self.db,
            )
        series = MODULE.query_series(
            "1w",
            points=82,
            now=datetime(2026, 8, 23, 0, 0, tzinfo=timezone.utc),
            path=self.db,
        )
        self.assertEqual(len(series["samples"]), 3)
        self.assertTrue(all(sample["bucket_seconds"] == 86400 for sample in series["samples"]))

    def test_downsample_never_exceeds_point_budget(self):
        end = datetime(2026, 8, 23, 4, 20, tzinfo=timezone.utc)
        base = int(datetime(2026, 8, 23, 4, 0, tzinfo=timezone.utc).timestamp())
        for minute in range(20):
            MODULE.upsert_sample(base + minute * 60, 60, minute + 1, minute, path=self.db)
        series = MODULE.query_series("1h", points=5, now=end, path=self.db)
        self.assertLessEqual(len(series["samples"]), 5)

    def test_one_year_aggregation_uses_display_columns_for_regular_history(self):
        start = datetime(2025, 8, 24, tzinfo=timezone.utc)
        for day in range(365):
            MODULE.upsert_sample(
                int(start.timestamp()) + day * 86400,
                86400,
                86400 + day,
                8640,
                source="backfill-day",
                path=self.db,
            )
        series = MODULE.query_series(
            "1y",
            points=96,
            now=datetime(2026, 8, 24, tzinfo=timezone.utc),
            path=self.db,
        )
        self.assertLessEqual(len(series["samples"]), 96)
        self.assertTrue(any(sample["bucket_seconds"] > 86400 for sample in series["samples"]))

    def test_sparse_one_year_history_keeps_real_gaps_when_budget_is_small(self):
        start = datetime(2025, 8, 24, tzinfo=timezone.utc)
        MODULE.upsert_sample(start, 86400, 100, 10, path=self.db)
        MODULE.upsert_sample(
            int(start.timestamp()) + 180 * 86400,
            86400,
            200,
            20,
            path=self.db,
        )
        series = MODULE.query_series(
            "1y",
            points=1,
            now=datetime(2026, 8, 24, tzinfo=timezone.utc),
            path=self.db,
        )
        self.assertEqual(len(series["samples"]), 2)
        self.assertTrue(all(sample["bucket_seconds"] == 86400 for sample in series["samples"]))

    def test_claim_due_is_atomic_interval_gate(self):
        self.assertTrue(MODULE.claim_due("backfill", 3600, path=self.db))
        self.assertFalse(MODULE.claim_due("backfill", 3600, path=self.db))


if __name__ == "__main__":
    unittest.main()
