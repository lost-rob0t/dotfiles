#!/usr/bin/env python3
"""Tests for the Qtile OpenRouter status helper."""

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
    def test_credit_color_thresholds(self):
        self.assertEqual(MODULE.credit_color(4.99), MODULE.RED)
        self.assertEqual(MODULE.credit_color(5.0), MODULE.YELLOW)
        self.assertEqual(MODULE.credit_color(9.99), MODULE.YELLOW)
        self.assertEqual(MODULE.credit_color(10.0), MODULE.GREEN)

    def test_compact_count(self):
        self.assertEqual(MODULE.compact_count(999), "999")
        self.assertEqual(MODULE.compact_count(1_200), "1.2k")
        self.assertEqual(MODULE.compact_count(12_000_000), "12M")
        self.assertEqual(MODULE.compact_count(2_500_000_000), "2.5B")

    def test_parse_credits_subtracts_usage(self):
        payload = {"data": {"total_credits": 25.0, "total_usage": 17.25}}
        self.assertEqual(MODULE.parse_credits(payload), 7.75)

    def test_parse_credits_never_goes_negative(self):
        payload = {"data": {"total_credits": 5.0, "total_usage": 8.0}}
        self.assertEqual(MODULE.parse_credits(payload), 0.0)

    def test_parse_token_totals_sums_rows(self):
        payload = {
            "data": {
                "data": [
                    {"tokens_prompt": 100, "tokens_completion": 10},
                    {"tokens_prompt": 250, "tokens_completion": 25},
                ]
            }
        }
        self.assertEqual(MODULE.parse_token_totals(payload), (350, 35))

    def test_render_contains_credit_live_io_and_rolling_total(self):
        status = MODULE.Status(
            credits_remaining=7.5,
            live_input_tokens=12_300,
            live_output_tokens=4_500,
            rolling_input_tokens=40_000_000,
            rolling_output_tokens=10_000_000,
        )
        rendered = MODULE.render(status)
        self.assertIn(MODULE.AI_ICON, rendered)
        self.assertIn(MODULE.YELLOW, rendered)
        self.assertIn("$7.50", rendered)
        self.assertIn("↓12.3k/m", rendered)
        self.assertIn("↑4.5k/m", rendered)
        self.assertIn("30d 50M", rendered)

    def test_management_key_prefers_dedicated_environment_variable(self):
        with mock.patch.dict(
            os.environ,
            {
                "OPENROUTER_MANAGEMENT_KEY": "management",
                "OPENROUTER_API_KEY": "regular",
            },
            clear=True,
        ):
            self.assertEqual(MODULE.load_management_key(), "management")

    def test_management_key_falls_back_to_file(self):
        with tempfile.TemporaryDirectory() as directory:
            key_file = Path(directory) / "management-key"
            key_file.write_text("file-key\n", encoding="utf-8")
            with mock.patch.dict(
                os.environ,
                {"OPENROUTER_MANAGEMENT_KEY_FILE": str(key_file)},
                clear=True,
            ):
                self.assertEqual(MODULE.load_management_key(), "file-key")

    def test_fetch_status_reuses_fresh_rolling_total(self):
        cached = MODULE.Status(
            credits_remaining=12.0,
            live_input_tokens=1,
            live_output_tokens=2,
            rolling_input_tokens=1_000,
            rolling_output_tokens=500,
        )
        cache = {
            "rolling_updated_at": MODULE.time.time(),
            "status": MODULE.asdict(cached),
        }
        with (
            mock.patch.object(MODULE, "fetch_credits", return_value=11.0),
            mock.patch.object(MODULE, "fetch_tokens", return_value=(10, 20)) as fetch_tokens,
        ):
            status, _ = MODULE.fetch_status("key", cache)

        self.assertEqual(fetch_tokens.call_count, 1)
        self.assertEqual(status.live_input_tokens, 10)
        self.assertEqual(status.live_output_tokens, 20)
        self.assertEqual(status.rolling_tokens, 1_500)

    def test_fetch_status_refreshes_stale_rolling_total(self):
        cached = MODULE.Status(
            credits_remaining=12.0,
            live_input_tokens=1,
            live_output_tokens=2,
            rolling_input_tokens=1_000,
            rolling_output_tokens=500,
        )
        cache = {
            "rolling_updated_at": 0,
            "status": MODULE.asdict(cached),
        }
        with (
            mock.patch.object(MODULE, "fetch_credits", return_value=11.0),
            mock.patch.object(
                MODULE,
                "fetch_tokens",
                side_effect=[(10, 20), (2_000, 1_000)],
            ) as fetch_tokens,
        ):
            status, rolling_updated_at = MODULE.fetch_status("key", cache)

        self.assertEqual(fetch_tokens.call_count, 2)
        self.assertGreater(rolling_updated_at, 0)
        self.assertEqual(status.rolling_tokens, 3_000)


if __name__ == "__main__":
    unittest.main()
