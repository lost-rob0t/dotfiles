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
        self.assertEqual(MODULE.compact_count(2_500_000_000), "2.5B")

    def test_uses_current_nerd_font_brain_codepoint(self):
        self.assertEqual(ord(MODULE.AI_ICON), 0xF09D1)
        self.assertFalse(0xF500 <= ord(MODULE.AI_ICON) <= 0xFD46)

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

    def test_render_labels_tokens_per_minute(self):
        status = MODULE.Status(
            input_tokens_per_minute=12_300,
            output_tokens_per_minute=4_500,
        )
        rendered = MODULE.render(status)
        self.assertIn(MODULE.AI_ICON, rendered)
        self.assertIn("12.3k↓/m", rendered)
        self.assertIn("4.5k↑/m", rendered)
        self.assertNotIn("$", rendered)
        self.assertNotIn("30d:", rendered)

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

    def test_fetch_tokens_requests_prompt_and_completion_metrics(self):
        start = MODULE.datetime(2026, 8, 22, 1, 0, tzinfo=MODULE.timezone.utc)
        end = MODULE.datetime(2026, 8, 22, 1, 1, tzinfo=MODULE.timezone.utc)
        response = {
            "data": {
                "data": [
                    {"tokens_prompt": 1_000, "tokens_completion": 100},
                ]
            }
        }
        with mock.patch.object(MODULE, "_request_json", return_value=response) as request:
            self.assertEqual(MODULE.fetch_tokens("key", start, end), (1_000, 100))

        payload = request.call_args.args[3]
        self.assertEqual(payload["metrics"], ["tokens_prompt", "tokens_completion"])
        self.assertNotIn("granularity", payload)

    def test_fetch_status_is_a_rolling_sixty_second_rate(self):
        with mock.patch.object(MODULE, "fetch_tokens", return_value=(10_000, 500)) as fetch_tokens:
            status = MODULE.fetch_status("key")

        _, start, end = fetch_tokens.call_args.args
        self.assertAlmostEqual((end - start).total_seconds(), 60.0)
        self.assertEqual(status.input_tokens_per_minute, 10_000)
        self.assertEqual(status.output_tokens_per_minute, 500)
        self.assertEqual(status.total_tokens_per_minute, 10_500)

    def test_status_cache_bounds_api_polling(self):
        status = MODULE.Status(
            input_tokens_per_minute=1_000,
            output_tokens_per_minute=100,
        )
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
