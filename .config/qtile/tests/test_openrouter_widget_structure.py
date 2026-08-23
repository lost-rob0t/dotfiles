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
    def test_one_second_render_poll_and_sparse_graph_capacity(self):
        self.assertEqual(assigned_constant("POLL_SECONDS"), 1)
        self.assertEqual(assigned_constant("GRAPH_SAMPLES"), 300)

    def test_credit_rate_graph_and_rotating_metrics_exist(self):
        classes = {node.name for node in TREE.body if isinstance(node, ast.ClassDef)}
        self.assertIn("OpenRouterCredit", classes)
        self.assertIn("OpenRouterRate", classes)
        self.assertIn("OpenRouterIOGraph", classes)
        self.assertIn("OpenRouterRotatingMetric", classes)

    def test_credit_thresholds_are_preserved(self):
        self.assertIn("balance < 5", SOURCE_TEXT)
        self.assertIn("balance < 10", SOURCE_TEXT)
        self.assertIn('name="openrouter_credit"', SOURCE_TEXT)

    def test_tokens_rotate_month_week_day_hour_and_spend_day_week_month(self):
        for value in ("tokens_month", "tokens_week", "tokens_day", "tokens_hour"):
            self.assertIn(value, SOURCE_TEXT)
        for value in ("spend_day", "spend_week", "spend_month"):
            self.assertIn(value, SOURCE_TEXT)
        self.assertIn('"Button4": self.previous', SOURCE_TEXT)
        self.assertIn('"Button5": self.next', SOURCE_TEXT)

    def test_rate_poll_respects_status_cache(self):
        self.assertIn('["python3", script, "--json"]', SOURCE_TEXT)
        self.assertNotIn('"--force"', SOURCE_TEXT)

    def test_graph_only_appends_changed_trusted_sample(self):
        self.assertIn("sample != self._last_sample", SOURCE_TEXT)
        self.assertIn('status.get("window_end")', SOURCE_TEXT)
        self.assertIn("self._last_sample = sample", SOURCE_TEXT)

    def test_graph_uses_adaptive_range_and_hides_constant_series(self):
        self.assertIn("minimum = min(values)", SOURCE_TEXT)
        self.assertIn("maximum = max(values)", SOURCE_TEXT)
        self.assertIn("if maximum <= minimum:", SOURCE_TEXT)

    def test_sync_reload_uses_qtile_process_not_external_cli(self):
        self.assertIn("lazy.function(_sync_and_reload)", SOURCE_TEXT)
        self.assertIn("qtile.call_soon_threadsafe(qtile.reload_config)", SOURCE_TEXT)
        self.assertNotIn("qtile cmd-obj", SOURCE_TEXT)

    def test_installs_topology_control_layer(self):
        self.assertIn("from qtile_control import install_desktop_control", SOURCE_TEXT)


if __name__ == "__main__":
    unittest.main()
