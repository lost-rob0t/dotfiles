#!/usr/bin/env python3
"""Regression tests for the floating Variety thumbnail selector."""

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / ".config" / "qtile" / "qtile-ai.org"
CONFIG = ROOT / ".config" / "qtile" / "config.py"
SELECTOR_RULE = "Match(wm_class='Variety', title='Variety Images')"


class VarietySelectorTests(unittest.TestCase):
    def test_selector_is_floating_in_literate_source_and_generated_config(self):
        self.assertIn(SELECTOR_RULE, SOURCE.read_text(encoding="utf-8"))
        self.assertIn(SELECTOR_RULE, CONFIG.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
