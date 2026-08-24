#!/usr/bin/env python3
"""Behavioral/structural tests for the combined Qtile network graph."""

from __future__ import annotations

import importlib.util
import sys
import types
import unittest
from pathlib import Path
from unittest import mock

SOURCE = Path(__file__).resolve().parents[1] / "qtile_net_io.py"
SOURCE_TEXT = SOURCE.read_text(encoding="utf-8")


def _install_libqtile_stub() -> None:
    libqtile = types.ModuleType("libqtile")
    widget = types.ModuleType("libqtile.widget")
    base = types.ModuleType("libqtile.widget.base")

    class Widget:
        def __init__(self, width=0, **config):
            self.width = width
            self.height = config.get("height", 26)
            for key, value in config.items():
                setattr(self, key, value)

        def add_defaults(self, defaults):
            for name, value, _desc in defaults:
                if not hasattr(self, name):
                    setattr(self, name, value)

    class BackgroundPoll(Widget):
        pass

    base._Widget = Widget
    base.BackgroundPoll = BackgroundPoll
    base.ORIENTATION_HORIZONTAL = 1
    widget.base = base
    libqtile.widget = widget
    sys.modules["libqtile"] = libqtile
    sys.modules["libqtile.widget"] = widget
    sys.modules["libqtile.widget.base"] = base


_install_libqtile_stub()
SPEC = importlib.util.spec_from_file_location("qtile_net_io", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class NetIOGraphTests(unittest.TestCase):
    def test_color_contract_is_explicit(self):
        self.assertIn("Cyan/blue is incoming/download traffic", SOURCE_TEXT)
        self.assertIn("Pink/red is outgoing/upload traffic", SOURCE_TEXT)
        self.assertIn('download_color", "#00b8ff"', SOURCE_TEXT)
        self.assertIn('upload_color", "#f6019d"', SOURCE_TEXT)

    def test_one_widget_owns_both_series_and_shared_ceiling(self):
        graph = MODULE.NetIOGraph(samples=4)
        graph.download.extend([100.0, 200.0])
        graph.upload.extend([10.0, 20.0])
        self.assertEqual(list(graph.download), [100.0, 200.0])
        self.assertEqual(list(graph.upload), [10.0, 20.0])
        self.assertIn("max([*self.download, *self.upload, 0.0])", SOURCE_TEXT)

    def test_rate_sampling_uses_one_elapsed_interval_for_both_directions(self):
        graph = MODULE.NetIOGraph(samples=4)
        graph._last = (1000, 500)
        graph._last_at = 10.0
        with mock.patch.object(graph, "_read_bytes", return_value=(1600, 800)), \
             mock.patch.object(MODULE.time, "monotonic", return_value=12.0), \
             mock.patch.object(graph, "draw"):
            graph._update()
        self.assertEqual(list(graph.download), [300.0])
        self.assertEqual(list(graph.upload), [150.0])

    def test_numeric_rate_reuses_rx_tx_direction_and_elapsed_interval(self):
        rate = MODULE.NetIORate()
        with mock.patch.object(MODULE.NetIOGraph, "_read_bytes", side_effect=[(1000, 500), (1600, 800)]), \
             mock.patch.object(MODULE.time, "monotonic", side_effect=[10.0, 12.0]):
            self.assertEqual(rate.poll(), "↓0B/s ↑0B/s")
            self.assertEqual(rate.poll(), "↓300B/s ↑150B/s")

    def test_numeric_rate_clamps_counter_resets(self):
        rate = MODULE.NetIORate()
        with mock.patch.object(MODULE.NetIOGraph, "_read_bytes", side_effect=[(1000, 500), (10, 5)]), \
             mock.patch.object(MODULE.time, "monotonic", side_effect=[10.0, 12.0]):
            rate.poll()
            self.assertEqual(rate.poll(), "↓0B/s ↑0B/s")

    def test_default_route_detection_ignores_loopback_fallback(self):
        route = "Iface Destination Gateway Flags RefCnt Use Metric Mask MTU Window IRTT\neth0 00000000 01020304 0003 0 0 0 00000000 0 0 0\n"
        dev = "Inter-| Receive | Transmit\n face |bytes packets errs drop fifo frame compressed multicast|bytes packets errs drop fifo colls carrier compressed\nlo: 999 0 0 0 0 0 0 0 999 0 0 0 0 0 0 0\neth0: 100 0 0 0 0 0 0 0 200 0 0 0 0 0 0 0\n"

        def fake_read_text(path, **_kwargs):
            return route if str(path).endswith("/proc/net/route") else dev

        with mock.patch.object(MODULE.Path, "read_text", autospec=True, side_effect=fake_read_text):
            self.assertEqual(MODULE.NetIOGraph._default_interface(), "eth0")
            self.assertEqual(MODULE.NetIOGraph._read_bytes(), (100, 200))


if __name__ == "__main__":
    unittest.main()
