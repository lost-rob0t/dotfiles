#!/usr/bin/env python3
"""Regression checks for the canonical Dunst history configuration."""

from pathlib import Path
import unittest

CONFIG_ROOT = Path(__file__).resolve().parents[2]
ORG = (CONFIG_ROOT / "dunst" / "dunst.org").read_text(encoding="utf-8")
CONFIG = (CONFIG_ROOT / "dunst" / "dunstrc").read_text(encoding="utf-8")


class DunstHistoryConfigTests(unittest.TestCase):
    def test_unlimited_history_is_canonical_and_tangled(self):
        for text in (ORG, CONFIG):
            self.assertIn("sticky_history = yes", text)
            self.assertIn("history_length = 0", text)
            self.assertNotIn("history_length = 20", text)


if __name__ == "__main__":
    unittest.main()
