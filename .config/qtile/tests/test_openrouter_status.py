#!/usr/bin/env python3
"""Tests for the Qtile OpenRouter token-rate helper."""

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

    def test_uses_current_nerd_font_brain_codepoint(self):
        self.assertEqual(ord(MODULE.AI_ICON), 0xF09D1)

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

    def test_closed_window_is_previous_full_minute(self):
        now = MODULE.datetime(2026, 8, 22, 21, 10, 47, 900, tzinfo=MODULE.timezone.utc)
        start, end = MODULE._closed_minute_window(now)
        self.assertEqual(end, MODULE.datetime(2026, 8, 22, 21, 10, tzinfo=MODULE.timezone.utc))
        self.assertEqual((end - start).total_seconds(), 60)

    def test_fetch_tokens_requests_minute_granularity(self):
        start = MODULE.datetime(2026, 8, 22, 21, 9, tzinfo=MODULE.timezone.utc)
        end = MODULE.datetime(2026, 8, 22, 21, 10, tzinfo=MODULE.timezone.utc)
        response = {"data": {"data": [{"tokens_prompt": 1_000, "tokens_completion": 100}]}}
        with mock.patch.object(MODULE, "_request_json", return_value=response) as request:
            self.assertEqual(MODULE.fetch_tokens("key", start, end), (1_000, 100))
        payload = request.call_args.args[3]
        self.assertEqual(payload["granularity"], "minute")
        self.assertEqual(payload["metrics"], ["tokens_prompt", "tokens_completion"])

    def test_fetch_balance_uses_total_credits_minus_usage(self):
        response = {"data": {"total_credits": 25.0, "total_usage": 7.25}}
        with mock.patch.object(MODULE, "_request_json", return_value=response) as request:
            self.assertEqual(MODULE.fetch_balance("key"), 17.75)
        self.assertEqual(request.call_args.args[:3], ("GET", "/credits", "key"))

    def test_165m_style_spike_is_rejected(self):
        with mock.patch.dict(os.environ, {"OPENROUTER_TPM_MAX": "10000000"}, clear=False):
            with self.assertRaisesRegex(RuntimeError, "implausible"):
                MODULE.validate_rate(165_000_000, 1_000)

    def test_tpm_guard_is_configurable(self):
        with mock.patch.dict(os.environ, {"OPENROUTER_TPM_MAX": "200000000"}, clear=False):
            MODULE.validate_rate(165_000_000, 1_000)

    def test_fetch_status_keeps_previous_balance_if_credit_poll_fails(self):
        previous = MODULE.Status(100, 10, balance_usd=9.50)
        with (
            mock.patch.object(MODULE, "fetch_tokens", return_value=(1_000, 100)),
            mock.patch.object(MODULE, "fetch_balance", side_effect=RuntimeError("offline")),
        ):
            status = MODULE.fetch_status("key", previous)
        self.assertEqual(status.balance_usd, 9.50)
        self.assertEqual(status.total_tokens_per_minute, 1_100)
        self.assertIsNotNone(status.window_end)

    def test_management_key_prefers_dedicated_environment_variable(self):
        with mock.patch.dict(
            os.environ,
            {"OPENROUTER_MANAGEMENT_KEY": "management", "OPENROUTER_API_KEY": "regular"},
            clear=True,
        ):
            self.assertEqual(MODULE.load_management_key(), "management")

    def test_status_cache_bounds_api_polling(self):
        status = MODULE.Status(1_000, 100, balance_usd=12.0, window_end="2026-08-22T21:10:00Z")
        with tempfile.TemporaryDirectory() as directory:
            with (
                mock.patch.dict(os.environ, {"XDG_CACHE_HOME": directory}),
                mock.patch.object(MODULE, "fetch_status", return_value=status) as fetch_status,
            ):
                first, first_stale = MODULE.status_with_cache("key")
                second, second_stale = MODULE.status_with_cache("key")
        self.assertEqual(first, status)
        self.assertEqual(second, status)
        self.assertFalse(first_stale)
        self.assertFalse(second_stale)
        self.assertEqual(fetch_status.call_count, 1)


if __name__ == "__main__":
    unittest.main()
