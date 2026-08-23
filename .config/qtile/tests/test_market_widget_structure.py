#!/usr/bin/env python3
"""Static regression tests for provider-neutral market stubs."""

from __future__ import annotations

import ast
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile_market.py"
TEXT = SOURCE.read_text(encoding="utf-8")
TREE = ast.parse(TEXT)


class MarketWidgetTests(unittest.TestCase):
    def test_expected_timeframes_exist(self):
        for timeframe in ("1m", "5m", "15m", "1h", "4h", "1d"):
            self.assertIn(f'"{timeframe}"', TEXT)

    def test_many_commodity_stubs_exist_without_prices(self):
        for symbol in ("GOLD", "SILV", "WTI", "BRENT", "NG", "COPPER", "CORN", "WHEAT", "SOY", "COFFEE", "COCOA"):
            self.assertIn(f'"{symbol}"', TEXT)
        self.assertNotIn('"price":', TEXT)

    def test_missing_provider_data_renders_dash_and_empty_graph(self):
        self.assertIn('return f"{symbol} {self._timeframe} —", self.muted', TEXT)
        self.assertIn("if len(series) >= 2:", TEXT)

    def test_provider_cache_is_outside_git(self):
        self.assertIn('"QTILE_MARKET_CACHE"', TEXT)
        self.assertIn('return root / "qtile" / "markets.json"', TEXT)


if __name__ == "__main__":
    unittest.main()
