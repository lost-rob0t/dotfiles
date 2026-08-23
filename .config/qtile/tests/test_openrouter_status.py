#!/usr/bin/env python3
"""Tests for the Qtile OpenRouter telemetry helper."""

from __future__ import annotations

import importlib.util
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "openrouter_status.py"
SPEC = importlib.util.spec_from_file_location("openrouter_status", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class OpenRouterStatusTests(unittest.TestCase):
    def test_compact_count(self):
        self.assertEqual(MODULE.compact_count(999), "999")
        self.assertEqual(MODULE.compact_count(1_200), "1.2k")
        self.assertEqual(MODULE.compact_count(12_000_000), "12M")

    def test_parse_token_totals_deduplicates_identical_buckets(self):
        row = {
            "created_at__minute": "2026-08-22T21:10:00Z",
            "tokens_prompt": 100,
            "tokens_completion": 10,
        }
        payload = {"data": {"data": [row, dict(row)]}}
        self.assertEqual(MODULE.parse_token_totals(payload), (100, 10))

    def test_parse_token_totals_rejects_truncated_analytics(self):
        payload = {"metadata": {"truncated": True}, "data": {"data": []}}
        with self.assertRaisesRegex(RuntimeError, "truncated"):
            MODULE.parse_token_totals(payload)

    def test_parse_usage_summary(self):
        payload = {
            "data": {
                "data": [
                    {"tokens_total": 100, "total_usage": 0.1},
                    {"tokens_total": 50, "total_usage": 0.2},
                ]
            }
        }
        tokens, spend = MODULE.parse_usage_summary(payload)
        self.assertEqual(tokens, 150)
        self.assertAlmostEqual(spend, 0.3)

    def test_period_starts_respect_local_timezone(self):
        tz = MODULE.timezone(MODULE.timedelta(hours=-4))
        now = MODULE.datetime(2026, 8, 22, 18, 57, tzinfo=tz)
        starts = MODULE.period_starts(now)
        self.assertEqual(starts["month"], MODULE.datetime(2026, 8, 1, tzinfo=tz))
        self.assertEqual(starts["week"], MODULE.datetime(2026, 8, 17, tzinfo=tz))
        self.assertEqual(starts["day"], MODULE.datetime(2026, 8, 22, tzinfo=tz))
        self.assertEqual(starts["hour"], MODULE.datetime(2026, 8, 22, 18, tzinfo=tz))

    def test_fetch_usage_range_uses_supported_metrics(self):
        start = MODULE.datetime(2026, 8, 22, tzinfo=MODULE.timezone.utc)
        end = MODULE.datetime(2026, 8, 22, 1, tzinfo=MODULE.timezone.utc)
        response = {"data": {"data": [{"tokens_total": 100, "total_usage": 0.5}]}}
        with mock.patch.object(MODULE, "_request_json", return_value=response) as request:
            self.assertEqual(MODULE.fetch_usage_range("key", start, end), (100, 0.5))
        query = request.call_args.args[3]
        self.assertEqual(query["metrics"], ["tokens_total", "total_usage"])
        self.assertEqual(query["granularity"], "hour")

    def test_fetch_period_totals_queries_month_week_day_hour(self):
        tz = MODULE.timezone(MODULE.timedelta(hours=-4))
        now = MODULE.datetime(2026, 8, 22, 18, 57, tzinfo=tz)
        with mock.patch.object(MODULE, "fetch_usage_range", return_value=(100, 0.25)) as fetch:
            result = MODULE.fetch_period_totals("key", now)
        self.assertEqual(fetch.call_count, 4)
        self.assertEqual(result["tokens_month"], 100)
        self.assertEqual(result["tokens_hour"], 100)
        self.assertEqual(result["spend_day"], 0.25)
        self.assertNotIn("spend_hour", result)

    def test_closed_window_is_previous_full_minute(self):
        now = MODULE.datetime(2026, 8, 22, 21, 10, 47, 900, tzinfo=MODULE.timezone.utc)
        start, end = MODULE._closed_minute_window(now)
        self.assertEqual(end, MODULE.datetime(2026, 8, 22, 21, 10, tzinfo=MODULE.timezone.utc))
        self.assertEqual((end - start).total_seconds(), 60)

    def test_fetch_balance_uses_total_credits_minus_usage(self):
        response = {"data": {"total_credits": 25.0, "total_usage": 7.25}}
        with mock.patch.object(MODULE, "_request_json", return_value=response):
            self.assertEqual(MODULE.fetch_balance("key"), 17.75)

    def test_165m_style_spike_is_rejected(self):
        with mock.patch.dict(os.environ, {"OPENROUTER_TPM_MAX": "10000000"}, clear=False):
            with self.assertRaisesRegex(RuntimeError, "implausible"):
                MODULE.validate_rate(165_000_000, 1_000)

    def test_same_closed_minute_does_not_refetch_rate(self):
        previous = MODULE.Status(
            100,
            10,
            window_end=MODULE._closed_minute_window()[1].isoformat().replace("+00:00", "Z"),
            usage_fetched_at=MODULE._iso_now(MODULE.datetime.now(MODULE.timezone.utc)),
            balance_fetched_at=MODULE._iso_now(MODULE.datetime.now(MODULE.timezone.utc)),
        )
        with (
            mock.patch.object(MODULE, "fetch_tokens") as fetch_tokens,
            mock.patch.object(MODULE, "fetch_period_totals") as fetch_period,
            mock.patch.object(MODULE, "fetch_balance") as fetch_balance,
        ):
            status = MODULE.fetch_status("key", previous)
        self.assertEqual(status.input_tokens_per_minute, 100)
        fetch_tokens.assert_not_called()
        fetch_period.assert_not_called()
        fetch_balance.assert_not_called()

    def test_status_cache_bounds_process_polling(self):
        status = MODULE.Status(1_000, 100, balance_usd=12.0, window_end="2026-08-22T21:10:00Z")
        with tempfile.TemporaryDirectory() as directory:
            with (
                mock.patch.dict(os.environ, {"XDG_CACHE_HOME": directory}),
                mock.patch.object(MODULE, "fetch_status", return_value=status) as fetch_status,
            ):
                first, _ = MODULE.status_with_cache("key")
                second, _ = MODULE.status_with_cache("key")
        self.assertEqual(first, status)
        self.assertEqual(second, status)
        self.assertEqual(fetch_status.call_count, 1)


if __name__ == "__main__":
    unittest.main()
