#!/usr/bin/env python3
"""Structural regression tests for the Qtile OpenRouter widget cluster."""

from __future__ import annotations

import ast
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile_openrouter.py"
SOURCE_TEXT = SOURCE.read_text(encoding="utf-8")
TREE = ast.parse(SOURCE_TEXT)


def assigned_constant(name):
    for node in TREE.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == name:
                    return ast.literal_eval(node.value)
    raise AssertionError(f"missing constant {name}")


class OpenRouterWidgetStructureTests(unittest.TestCase):
    def test_five_second_rate_polling_and_five_minute_graph(self):
        self.assertEqual(assigned_constant("POLL_SECONDS"), 5)
        self.assertEqual(assigned_constant("GRAPH_SAMPLES"), 60)

    def test_readable_rate_font_size(self):
        self.assertGreaterEqual(assigned_constant("RATE_FONTSIZE"), 12)

    def test_rate_and_graph_widgets_exist_without_credit_or_rolling_widgets(self):
        classes = {
            node.name
            for node in TREE.body
            if isinstance(node, ast.ClassDef)
        }
        self.assertIn("OpenRouterRate", classes)
        self.assertIn("OpenRouterIOGraph", classes)
        self.assertNotIn("OpenRouterCredit", classes)
        self.assertNotIn("OpenRouterCacheText", classes)

    def test_rate_poll_respects_status_cache(self):
        self.assertIn('["python3", script, "--json"]', SOURCE_TEXT)
        self.assertNotIn('"--force"', SOURCE_TEXT)
        self.assertIn('"openrouter_io_graph"', SOURCE_TEXT)
        self.assertNotIn('"rolling"', SOURCE_TEXT)

    def test_rate_is_rendered_as_two_tokens_per_minute_lines(self):
        self.assertIn("{incoming}↓/m</span>\\n", SOURCE_TEXT)
        self.assertIn("{outgoing}↑/m{stale}</span>", SOURCE_TEXT)

    def test_graph_does_not_seed_fake_zero_history(self):
        self.assertIn("deque(maxlen=self.samples)", SOURCE_TEXT)
        self.assertNotIn("[0.0] * self.samples", SOURCE_TEXT)

    def test_graph_uses_adaptive_range_and_hides_constant_series(self):
        self.assertIn("minimum = min(values)", SOURCE_TEXT)
        self.assertIn("maximum = max(values)", SOURCE_TEXT)
        self.assertIn("if maximum <= minimum:", SOURCE_TEXT)
        self.assertIn("return", SOURCE_TEXT)

    def test_sync_reload_uses_qtile_process_not_external_cli(self):
        self.assertIn("lazy.function(_sync_and_reload)", SOURCE_TEXT)
        self.assertIn("qtile.call_soon_threadsafe(qtile.reload_config)", SOURCE_TEXT)
        self.assertNotIn("qtile cmd-obj", SOURCE_TEXT)

    def test_sync_reload_notifies_user(self):
        self.assertIn('shutil.which("dunstify")', SOURCE_TEXT)
        self.assertIn('shutil.which("notify-send")', SOURCE_TEXT)
        self.assertIn('"Dotfiles sync failed"', SOURCE_TEXT)
        self.assertIn('"Dotfiles synced"', SOURCE_TEXT)


if __name__ == "__main__":
    unittest.main()
