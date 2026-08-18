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
    def test_one_hz_polling_and_one_minute_graph(self):
        self.assertEqual(assigned_constant("POLL_SECONDS"), 1)
        self.assertEqual(assigned_constant("GRAPH_SAMPLES"), 60)

    def test_readable_font_sizes(self):
        self.assertGreaterEqual(assigned_constant("CREDIT_FONTSIZE"), 14)
        self.assertGreaterEqual(assigned_constant("IO_FONTSIZE"), 12)
        self.assertGreaterEqual(assigned_constant("ROLLING_FONTSIZE"), 13)

    def test_graph_widget_exists(self):
        classes = {
            node.name
            for node in TREE.body
            if isinstance(node, ast.ClassDef)
        }
        self.assertIn("OpenRouterIOGraph", classes)
        self.assertIn("OpenRouterCredit", classes)
        self.assertIn("OpenRouterCacheText", classes)

    def test_live_poll_forces_short_cache_but_not_rolling_window(self):
        self.assertIn('"--json", "--force"', SOURCE_TEXT)
        self.assertIn('"openrouter_io_graph"', SOURCE_TEXT)
        self.assertIn('"rolling"', SOURCE_TEXT)

    def test_io_is_rendered_as_two_lines(self):
        self.assertIn("{incoming}↓</span>\\n", SOURCE_TEXT)
        self.assertIn("{outgoing}↑</span>", SOURCE_TEXT)

    def test_graph_scales_each_series_independently(self):
        self.assertIn("maximum = max(max(values, default=0), 1)", SOURCE_TEXT)
        self.assertNotIn("max(self.input_values, default=0)", SOURCE_TEXT)
        self.assertNotIn("max(self.output_values, default=0)", SOURCE_TEXT)

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
