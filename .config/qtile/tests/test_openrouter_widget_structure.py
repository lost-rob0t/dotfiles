#!/usr/bin/env python3
"""Structural regression tests for the Qtile OpenRouter widget cluster."""

from __future__ import annotations

import ast
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile_openrouter.py"
TREE = ast.parse(SOURCE.read_text(encoding="utf-8"))


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
        source = SOURCE.read_text(encoding="utf-8")
        self.assertIn('"--json", "--force"', source)
        self.assertIn('"openrouter_io_graph"', source)
        self.assertIn('"rolling"', source)

    def test_io_is_rendered_as_two_lines(self):
        source = SOURCE.read_text(encoding="utf-8")
        self.assertIn("{incoming}↓</span>\\n", source)
        self.assertIn("{outgoing}↑</span>", source)


if __name__ == "__main__":
    unittest.main()
