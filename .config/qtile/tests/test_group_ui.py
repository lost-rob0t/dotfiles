#!/usr/bin/env python3
"""Regression tests for the visible Qtile group labels."""

from __future__ import annotations

import ast
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
CONFIG = ROOT / ".config" / "qtile" / "config.py"
SOURCE = ROOT / ".config" / "qtile" / "qtile-ai.org"
AI_ICON = "󰚩"


def _config_labels() -> list[str]:
    text = CONFIG.read_text(encoding="utf-8")
    tree = ast.parse(text)
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
            isinstance(target, ast.Name) and target.id == "group_labels"
            for target in node.targets
        ):
            return ast.literal_eval(node.value)
    raise AssertionError("group_labels assignment not found")


def _source_labels() -> list[str]:
    text = SOURCE.read_text(encoding="utf-8")
    values = text.split("group_labels = [", 1)[1].split("]", 1)[0]
    return ast.literal_eval("[" + values + "]")


class GroupUiTests(unittest.TestCase):
    def test_group_three_uses_the_ai_icon_in_runtime_config(self):
        self.assertEqual(_config_labels()[2], AI_ICON)

    def test_group_three_uses_the_ai_icon_in_literate_source(self):
        self.assertEqual(_source_labels()[2], AI_ICON)


if __name__ == "__main__":
    unittest.main()
