"""Provider-neutral rotating market widgets for Qtile.

The widget never fabricates prices. Providers write trusted snapshots to the cache;
until then the built-in instrument catalogue renders as an explicit stub.
"""

from __future__ import annotations

import json
import os
from pathlib import Path

from libqtile.widget import base

TIMEFRAMES = ("1m", "5m", "15m", "1h", "4h", "1d")
COMMODITY_STUBS = (
    ("GOLD", "Gold"),
    ("SILV", "Silver"),
    ("WTI", "WTI Crude"),
    ("BRENT", "Brent Crude"),
    ("NG", "Natural Gas"),
    ("COPPER", "Copper"),
    ("CORN", "Corn"),
    ("WHEAT", "Wheat"),
    ("SOY", "Soybeans"),
    ("COFFEE", "Coffee"),
    ("COCOA", "Cocoa"),
    ("COTTON", "Cotton"),
    ("SUGAR", "Sugar"),
)
KALSHI_STUBS = (("KALSHI", "Kalshi"),)


def market_cache_path() -> Path:
    configured = os.environ.get("QTILE_MARKET_CACHE")
    if configured:
        return Path(configured).expanduser()
    root = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser()
    return root / "qtile" / "markets.json"


def load_market_cache(path: Path | None = None) -> dict:
    path = path or market_cache_path()
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, TypeError):
        return {}
    return payload if isinstance(payload, dict) else {}


def stub_entries(feed: str) -> list[dict]:
    catalogue = KALSHI_STUBS if feed == "kalshi" else COMMODITY_STUBS
    return [
        {
            "symbol": symbol,
            "label": label,
            "provider": "stub",
            "timeframes": {},
        }
        for symbol, label in catalogue
    ]


def feed_entries(payload: dict, feed: str) -> list[dict]:
    entries = payload.get(feed, [])
    if not isinstance(entries, list):
        return stub_entries(feed)
    valid = [
        entry
        for entry in entries
        if isinstance(entry, dict) and entry.get("symbol") and entry.get("label")
    ]
    return valid or stub_entries(feed)


def timeframe_snapshot(entry: dict, timeframe: str) -> dict:
    frames = entry.get("timeframes", {})
    if not isinstance(frames, dict):
        return {}
    snapshot = frames.get(timeframe, {})
    return snapshot if isinstance(snapshot, dict) else {}


class MarketCarousel(base._Widget):
    """Rotate instruments and timeframes while drawing any trusted cached series."""

    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ("feed", "commodities", "Feed key in the provider cache."),
        ("frequency", 4, "Rotation interval in seconds."),
        ("foreground", "#fba922", "Text colour."),
        ("graph_color", "#fba922", "Trend line colour."),
        ("accent", "#2de2e6", "Positive trend colour."),
        ("negative", "#dd546e", "Negative trend colour."),
        ("muted", "#92406e", "Missing-data colour."),
        ("line_width", 1.4, "Trend line width."),
        ("padding", 4, "Horizontal padding."),
    ]

    def __init__(self, width=220, **config):
        super().__init__(width, **config)
        self.add_defaults(self.defaults)
        self.index = 0
        self._entries = []
        self._snapshot = {}
        self._entry = {}
        self._timeframe = TIMEFRAMES[0]
        self.add_callbacks({"Button4": self.previous, "Button5": self.next})

    def _configure(self, qtile, bar_obj):
        super()._configure(qtile, bar_obj)
        self.layout = self.drawer.textlayout(
            "",
            self.foreground,
            "Hack Nerd Regular",
            11,
            None,
            markup=False,
        )

    def timer_setup(self):
        self._refresh()
        self.timeout_add(self.frequency, self.timer_setup)

    def previous(self):
        self.index -= 1
        self._refresh(rotate=False)

    def next(self):
        self.index += 1
        self._refresh(rotate=False)

    def _refresh(self, rotate=True):
        payload = load_market_cache()
        self._entries = feed_entries(payload, self.feed)
        slots = max(len(self._entries) * len(TIMEFRAMES), 1)
        self.index %= slots
        entry_index = self.index // len(TIMEFRAMES)
        frame_index = self.index % len(TIMEFRAMES)
        self._entry = self._entries[entry_index]
        self._timeframe = TIMEFRAMES[frame_index]
        self._snapshot = timeframe_snapshot(self._entry, self._timeframe)
        self.draw()
        if rotate:
            self.index = (self.index + 1) % slots

    def _display_text(self):
        symbol = str(self._entry.get("symbol", self.feed.upper()))
        price = self._snapshot.get("price")
        change = self._snapshot.get("change")
        if price is None:
            return f"{symbol} {self._timeframe} —", self.muted
        try:
            price_text = f"{float(price):g}"
        except (TypeError, ValueError):
            return f"{symbol} {self._timeframe} —", self.muted
        if change is None:
            return f"{symbol} {self._timeframe} {price_text}", self.foreground
        try:
            change_value = float(change)
        except (TypeError, ValueError):
            return f"{symbol} {self._timeframe} {price_text}", self.foreground
        colour = self.accent if change_value >= 0 else self.negative
        return f"{symbol} {self._timeframe} {price_text} {change_value:+.2f}%", colour

    def _series(self):
        raw = self._snapshot.get("series", [])
        if not isinstance(raw, list):
            return []
        values = []
        for value in raw:
            try:
                values.append(float(value))
            except (TypeError, ValueError):
                return []
        return values

    def draw(self):
        if not hasattr(self, "drawer"):
            return
        self.drawer.clear(self.background or self.bar.background)
        text, colour = self._display_text()
        self.layout.text = text
        self.layout.colour = colour
        text_width = int(self.width * 0.62)
        self.layout.width = text_width
        y = max((self.height - self.layout.height) / 2, 0)
        self.layout.draw(self.padding, y)

        series = self._series()
        if len(series) >= 2:
            minimum = min(series)
            maximum = max(series)
            if maximum > minimum:
                graph_left = max(text_width + self.padding * 2, int(self.width * 0.64))
                graph_right = self.width - self.padding
                graph_width = max(graph_right - graph_left, 1)
                graph_height = max(self.height - 6, 1)
                step = graph_width / max(len(series) - 1, 1)
                self.drawer.set_source_rgb(self.graph_color)
                self.drawer.ctx.set_line_width(self.line_width)
                for index, value in enumerate(series):
                    x = graph_left + index * step
                    normalized = (value - minimum) / (maximum - minimum)
                    y = self.height - 3 - normalized * graph_height
                    if index == 0:
                        self.drawer.ctx.move_to(x, y)
                    else:
                        self.drawer.ctx.line_to(x, y)
                self.drawer.ctx.stroke()

        self.draw_at_default_position()
