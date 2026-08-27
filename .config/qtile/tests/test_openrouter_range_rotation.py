#!/usr/bin/env python3
"""Behavioral tests for OpenRouter graph timeframe rotation and scaling."""

from __future__ import annotations

import importlib.util
import sys
import types
import unittest
from pathlib import Path
from unittest import mock

QTILE_DIR = Path(__file__).resolve().parents[1]
SOURCE = QTILE_DIR / "qtile_openrouter.py"
sys.path.insert(0, str(QTILE_DIR))


def _install_libqtile_stub() -> None:
    libqtile = types.ModuleType("libqtile")
    config = types.ModuleType("libqtile.config")
    lazy_module = types.ModuleType("libqtile.lazy")
    widget = types.ModuleType("libqtile.widget")
    base = types.ModuleType("libqtile.widget.base")

    class Key:
        pass

    class BackgroundPoll:
        def __init__(self, text="", **config):
            self.text = text
            self.mouse_callbacks = config.get("mouse_callbacks", {})

        def add_callbacks(self, defaults):
            defaults.update(self.mouse_callbacks)
            self.mouse_callbacks = defaults

        def force_update(self):
            return None

    class Widget:
        pass

    config.Key = Key
    lazy_module.lazy = types.SimpleNamespace()
    base.BackgroundPoll = BackgroundPoll
    base._Widget = Widget
    base.ORIENTATION_HORIZONTAL = 1
    widget.base = base
    libqtile.config = config
    libqtile.lazy = lazy_module
    libqtile.widget = widget

    sys.modules["libqtile"] = libqtile
    sys.modules["libqtile.config"] = config
    sys.modules["libqtile.lazy"] = lazy_module
    sys.modules["libqtile.widget"] = widget
    sys.modules["libqtile.widget.base"] = base


_install_libqtile_stub()
SPEC = importlib.util.spec_from_file_location("qtile_openrouter_rotation", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class OpenRouterRangeRotationTests(unittest.TestCase):
    def setUp(self):
        MODULE._graph_range_index = 0
        MODULE._graph_range_changed_at = 100.0
        MODULE._metric_rotation_paused = False

    def test_exact_requested_range_order(self):
        self.assertEqual(
            MODULE.GRAPH_RANGES,
            ("1m", "5m", "1h", "6h", "12h", "1d", "1w", "1mo", "1y"),
        )

    def test_rotation_changes_only_at_five_second_boundaries(self):
        self.assertEqual(MODULE._rotate_graph_range(104.999), "1m")
        self.assertEqual(MODULE._rotate_graph_range(105.0), "5m")
        self.assertEqual(MODULE._rotate_graph_range(109.999), "5m")
        self.assertEqual(MODULE._rotate_graph_range(110.0), "1h")

    def test_delayed_tick_catches_up_without_drift(self):
        self.assertEqual(MODULE._rotate_graph_range(117.0), "6h")
        self.assertEqual(MODULE._graph_range_changed_at, 115.0)
        self.assertEqual(MODULE._rotate_graph_range(120.0), "12h")

    def test_full_cycle_wraps_to_one_minute(self):
        self.assertEqual(MODULE._rotate_graph_range(145.0), "1m")
        self.assertEqual(MODULE._graph_range_index, 0)

    def test_manual_step_moves_both_directions_and_restarts_timer(self):
        with mock.patch.object(MODULE.time, "monotonic", return_value=222.0):
            self.assertEqual(MODULE._step_graph_range(1), "5m")
        self.assertEqual(MODULE._graph_range_changed_at, 222.0)
        with mock.patch.object(MODULE.time, "monotonic", return_value=223.0):
            self.assertEqual(MODULE._step_graph_range(-1), "1m")
        self.assertEqual(MODULE._graph_range_changed_at, 223.0)

    def test_log_scale_keeps_zero_at_baseline_and_ceiling_at_full_height(self):
        self.assertEqual(MODULE._graph_normalized(0, 10_000), 0.0)
        self.assertEqual(MODULE._graph_normalized(-1, 10_000), 0.0)
        self.assertAlmostEqual(MODULE._graph_normalized(10_000, 10_000), 1.0)

    def test_log_scale_keeps_small_output_visibly_above_subpixel_linear_scale(self):
        # 100 output vs 10k input would be 1% of half-height on a linear graph.
        # The shared log transform keeps both on the same mathematical scale but
        # compresses dynamic range so the smaller completion series is readable.
        normalized = MODULE._graph_normalized(100, 10_000)
        self.assertGreater(normalized, 0.45)
        self.assertLess(normalized, 0.55)

    def test_log_scale_is_monotonic_and_shared_for_both_series(self):
        values = [MODULE._graph_normalized(value, 100_000) for value in (0, 10, 100, 1_000, 10_000, 100_000)]
        self.assertEqual(values, sorted(values))
        self.assertEqual(len(set(values)), len(values))

    def test_bucket_bounds_preserve_provider_resolution(self):
        self.assertEqual(
            MODULE._bucket_bounds({"timestamp": 150.0, "bucket_seconds": 60}),
            (120.0, 180.0),
        )
        self.assertEqual(
            MODULE._bucket_bounds({"timestamp": 5_400.0, "bucket_seconds": 3_600}),
            (3_600.0, 7_200.0),
        )

    def test_missing_bucket_duration_never_creates_zero_width_sample(self):
        start, end = MODULE._bucket_bounds({"timestamp": 10.0, "bucket_seconds": 0})
        self.assertGreater(end, start)

    def test_metric_click_pauses_all_text_rotations_and_middle_click_resumes(self):
        colors = [str(index) for index in range(10)]
        tokens = MODULE.OpenRouterRotatingMetric("status", colors, "tokens")
        spend = MODULE.OpenRouterRotatingMetric("status", colors, "spend")
        self.assertEqual(
            set(tokens.mouse_callbacks),
            {"Button1", "Button2", "Button3", "Button4", "Button5"},
        )

        with mock.patch.object(tokens, "force_update") as refresh:
            tokens.activate()
            refresh.assert_called_once_with()
        self.assertTrue(MODULE._metric_rotation_paused_state())
        self.assertEqual(tokens.index, 1)

        initial = spend.index
        spend.last_rotate = 100.0
        with mock.patch.object(MODULE.time, "monotonic", return_value=100.0 + MODULE.ROTATE_SECONDS):
            spend._maybe_rotate()
        self.assertEqual(spend.index, initial)

        with mock.patch.object(spend, "force_update") as refresh:
            spend.resume()
            refresh.assert_called_once_with()
        self.assertFalse(MODULE._metric_rotation_paused_state())

    def test_graph_left_click_pauses_and_advances_once(self):
        colors = [str(index) for index in range(10)]
        graph = MODULE.OpenRouterGraphRange(colors)
        with mock.patch.object(graph, "force_update") as refresh:
            graph.activate()
            refresh.assert_called_once_with()
        self.assertTrue(MODULE._graph_rotation_paused_state())
        self.assertEqual(MODULE._graph_range_label(), "5m")

        with mock.patch.object(graph, "force_update") as refresh:
            graph.resume()
            refresh.assert_called_once_with()
        self.assertFalse(MODULE._graph_rotation_paused_state())


if __name__ == "__main__":
    unittest.main()
