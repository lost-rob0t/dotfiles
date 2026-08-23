#!/usr/bin/env python3
"""Regression tests for OpenRouter persistent-history backfill."""

from __future__ import annotations

import importlib.util
import sqlite3
import sys
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest import mock

QTILE_DIR = Path(__file__).resolve().parents[1]
SCRIPT = QTILE_DIR / "scripts" / "openrouter_status.py"
sys.path.insert(0, str(QTILE_DIR))
SPEC = importlib.util.spec_from_file_location("openrouter_status_backfill", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class OpenRouterBackfillTests(unittest.TestCase):
    def test_parse_history_rows_keeps_real_bucket_granularity(self):
        payload = {
            "data": {
                "data": [
                    {
                        "created_at__hour": "2026-08-23T03:00:00Z",
                        "tokens_prompt": 600,
                        "tokens_completion": 60,
                        "total_usage": 0.25,
                    }
                ]
            }
        }
        rows = MODULE.parse_history_rows(payload, "hour")
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["seconds"], 3600)
        self.assertEqual(rows[0]["input"], 600)
        self.assertEqual(rows[0]["output"], 60)
        self.assertAlmostEqual(rows[0]["spend"], 0.25)

    def test_history_query_requests_prompt_completion_and_spend(self):
        start = datetime(2026, 8, 22, tzinfo=timezone.utc)
        end = datetime(2026, 8, 23, tzinfo=timezone.utc)
        response = {"data": {"data": []}}
        with mock.patch.object(MODULE, "_request_json", return_value=response) as request:
            self.assertEqual(MODULE.fetch_history_range("key", start, end, "hour"), [])
        query = request.call_args.args[3]
        self.assertEqual(
            query["metrics"],
            ["tokens_prompt", "tokens_completion", "total_usage"],
        )
        self.assertEqual(query["granularity"], "hour")

    def test_backfill_uses_daily_old_history_and_hourly_recent_history(self):
        now = datetime(2026, 8, 23, 4, 38, tzinfo=timezone.utc)
        with (
            mock.patch.object(MODULE, "fetch_history_range", return_value=[]) as fetch,
            mock.patch.object(MODULE.history, "set_metadata"),
        ):
            result = MODULE.backfill_history("key", now)
        self.assertEqual(result, {"day": 0, "hour": 0})
        self.assertEqual(fetch.call_count, 2)
        granularities = [call.args[3] for call in fetch.call_args_list]
        self.assertEqual(granularities, ["day", "hour"])

    def test_new_closed_minute_is_persisted_once(self):
        now_end = MODULE._closed_minute_window()[1]
        previous = MODULE.Status(
            1,
            1,
            window_end=(now_end - MODULE.timedelta(minutes=1)).isoformat().replace("+00:00", "Z"),
            usage_fetched_at=MODULE._iso_now(MODULE.datetime.now(MODULE.timezone.utc)),
            balance_fetched_at=MODULE._iso_now(MODULE.datetime.now(MODULE.timezone.utc)),
        )
        with (
            mock.patch.object(MODULE, "fetch_tokens", return_value=(100, 10)),
            mock.patch.object(MODULE.history, "upsert_sample") as persist,
        ):
            status = MODULE.fetch_status("key", previous)
        self.assertEqual(status.input_tokens_per_minute, 100)
        self.assertEqual(status.output_tokens_per_minute, 10)
        persist.assert_called_once()
        self.assertEqual(persist.call_args.kwargs["source"], "live-minute")

    def test_history_db_failure_does_not_destroy_live_rate(self):
        now_end = MODULE._closed_minute_window()[1]
        previous = MODULE.Status(
            1,
            1,
            window_end=(now_end - MODULE.timedelta(minutes=1)).isoformat().replace("+00:00", "Z"),
            usage_fetched_at=MODULE._iso_now(MODULE.datetime.now(MODULE.timezone.utc)),
            balance_fetched_at=MODULE._iso_now(MODULE.datetime.now(MODULE.timezone.utc)),
        )
        with (
            mock.patch.object(MODULE, "fetch_tokens", return_value=(321, 45)),
            mock.patch.object(
                MODULE.history,
                "upsert_sample",
                side_effect=sqlite3.OperationalError("readonly database"),
            ),
        ):
            status = MODULE.fetch_status("key", previous)
        self.assertEqual(status.input_tokens_per_minute, 321)
        self.assertEqual(status.output_tokens_per_minute, 45)
        self.assertIn("history storage unavailable", status.last_error)
        self.assertIn("readonly database", status.last_error)

    def test_history_diagnostics_report_storage_error(self):
        with mock.patch.object(
            MODULE.history,
            "summary",
            side_effect=sqlite3.OperationalError("locked"),
        ):
            info = MODULE._history_diagnostics()
        self.assertEqual(info["rows"], 0)
        self.assertEqual(info["history_error"], "locked")

    def test_stale_cache_reports_provider_error_instead_of_silently_hiding_it(self):
        cached = MODULE.Status(100, 10, window_end="2026-08-23T04:00:00Z")
        fake_cache = {"fetched_at": 0, "status": MODULE.asdict(cached)}
        lock = mock.mock_open()
        with (
            mock.patch.object(MODULE, "_read_cache", return_value=fake_cache),
            mock.patch.object(MODULE, "fetch_status", side_effect=RuntimeError("OpenRouter rate limited")),
            mock.patch.object(MODULE, "_cache_path", return_value=Path("/tmp/fake-openrouter-status.json")),
            mock.patch.object(Path, "open", lock),
            mock.patch.object(MODULE.fcntl, "flock"),
        ):
            status, stale = MODULE.status_with_cache("key")
        self.assertTrue(stale)
        self.assertEqual(status.last_error, "OpenRouter rate limited")


if __name__ == "__main__":
    unittest.main()
