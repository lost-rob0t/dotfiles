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


def function_source(name):
    for node in TREE.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)) and node.name == name:
            return ast.get_source_segment(SOURCE_TEXT, node) or ""
    raise AssertionError(f"missing function {name}")


def method_source(class_name, method_name):
    for node in TREE.body:
        if isinstance(node, ast.ClassDef) and node.name == class_name:
            for child in node.body:
                if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef)) and child.name == method_name:
                    return ast.get_source_segment(SOURCE_TEXT, child) or ""
    raise AssertionError(f"missing method {class_name}.{method_name}")


class OpenRouterWidgetStructureTests(unittest.TestCase):
    def test_one_second_render_poll_and_graph_point_budget(self):
        self.assertEqual(assigned_constant("POLL_SECONDS"), 1)
        self.assertEqual(assigned_constant("GRAPH_SAMPLES"), 192)
        self.assertEqual(assigned_constant("ROTATE_SECONDS"), 5)
        self.assertGreaterEqual(assigned_constant("COLLECTOR_HEARTBEAT_STALE_SECONDS"), 15)

    def test_credit_rate_graph_range_graph_and_rotating_metrics_exist(self):
        classes = {node.name for node in TREE.body if isinstance(node, ast.ClassDef)}
        self.assertIn("OpenRouterCredit", classes)
        self.assertIn("OpenRouterRate", classes)
        self.assertIn("OpenRouterGraphRange", classes)
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

    def test_graph_uses_all_requested_persistent_ranges(self):
        self.assertIn("openrouter_history.TIMEFRAMES", SOURCE_TEXT)
        self.assertIn("_rotate_graph_range", SOURCE_TEXT)
        self.assertIn("_step_graph_range", SOURCE_TEXT)
        self.assertIn('name="openrouter_graph_range"', SOURCE_TEXT)
        self.assertIn('"Button4": self.previous', SOURCE_TEXT)
        self.assertIn('"Button5": self.next', SOURCE_TEXT)

    def test_graph_queries_local_history_instead_of_ram_deque(self):
        self.assertIn("openrouter_history.query_series", SOURCE_TEXT)
        self.assertNotIn("from collections import deque", SOURCE_TEXT)
        self.assertNotIn("self.input_values", SOURCE_TEXT)
        self.assertNotIn("self.output_values", SOURCE_TEXT)

    def test_graph_history_query_runs_off_qtile_event_loop(self):
        self.assertIn("self._query_running", SOURCE_TEXT)
        self.assertIn('name="qtile-openrouter-graph"', SOURCE_TEXT)
        self.assertIn("self.qtile.call_soon_threadsafe(apply)", SOURCE_TEXT)
        self.assertIn("daemon=True", SOURCE_TEXT)

    def test_graph_uses_one_shared_log_scale_for_both_series(self):
        self.assertIn('ceiling = float(self.series.get("ceiling") or 0)', SOURCE_TEXT)
        self.assertIn('_draw_series("input"', SOURCE_TEXT)
        self.assertIn('_draw_series("output"', SOURCE_TEXT)
        self.assertIn("_graph_normalized", SOURCE_TEXT)
        self.assertIn("math.log1p", function_source("_graph_normalized"))
        self.assertNotIn("minimum = min(values)", SOURCE_TEXT)
        self.assertNotIn("if maximum <= minimum:", SOURCE_TEXT)

    def test_graph_draws_provider_bucket_steps_without_gap_interpolation(self):
        source = method_source("OpenRouterIOGraph", "_draw_series")
        self.assertIn("_bucket_bounds(sample)", source)
        self.assertIn("clipped_start", source)
        self.assertIn("clipped_end", source)
        self.assertIn("contiguous", source)
        self.assertIn("self.drawer.ctx.line_to(x0, y)", source)
        self.assertIn("self.drawer.ctx.line_to(x1, y)", source)
        self.assertNotIn('float(sample["timestamp"]) - start', source)

    def test_long_range_budget_follows_pixel_width(self):
        source = method_source("OpenRouterIOGraph", "_update")
        self.assertIn("points=min(self.samples, max(int(self.width), 1))", source)

    def test_stacked_rate_uses_smaller_font_than_single_line_metrics(self):
        self.assertLess(assigned_constant("RATE_FONTSIZE"), assigned_constant("METRIC_FONTSIZE"))
        source = function_source("_telemetry_widgets")
        self.assertIn('"fontsize": METRIC_FONTSIZE', source)
        self.assertIn('"fontsize": RATE_FONTSIZE', source)
        self.assertIn("OpenRouterRate", source)

    def test_widget_poll_is_cache_only(self):
        source = function_source("_fetch_payload")
        self.assertIn("_read_cached_payload()", source)
        self.assertNotIn("subprocess.run", source)
        self.assertNotIn("subprocess.Popen", source)
        self.assertNotIn("--json", source)
        self.assertNotIn("--force", source)

    def test_qtile_directly_starts_detached_collector(self):
        source = function_source("_start_collector")
        self.assertIn("subprocess.Popen", source)
        self.assertIn('"--daemon"', source)
        self.assertIn('"--parent-pid"', source)
        self.assertIn("str(os.getpid())", source)
        self.assertIn("start_new_session=True", source)
        install = function_source("install_openrouter_widget")
        self.assertIn("_start_collector(script)", install)

    def test_local_cache_surfaces_collector_errors_and_staleness(self):
        source = function_source("_read_cached_payload")
        self.assertIn('cache.get("collector_error")', source)
        self.assertIn('cache.get("collector_heartbeat")', source)
        self.assertIn('payload["stale"]', source)
        self.assertIn('payload["last_error"]', source)

    def test_collector_errors_are_visible(self):
        self.assertIn('payload.get("last_error")', SOURCE_TEXT)
        self.assertIn('payload.get("usage_error")', SOURCE_TEXT)

    def test_sync_reload_uses_qtile_process_not_external_cli(self):
        self.assertIn("lazy.function(_sync_and_reload)", SOURCE_TEXT)
        self.assertIn("qtile.call_soon_threadsafe(qtile.reload_config)", SOURCE_TEXT)
        self.assertNotIn("qtile cmd-obj", SOURCE_TEXT)

    def test_installs_topology_control_layer(self):
        self.assertIn("from qtile_control import install_desktop_control", SOURCE_TEXT)


if __name__ == "__main__":
    unittest.main()
