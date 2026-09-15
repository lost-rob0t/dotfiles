"""Exact API bucket geometry and real Cairo drawing; no live WM required."""
from __future__ import annotations

from pathlib import Path
import re
import sys
import types
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from llm_log_graph import RANGES, TimelineFeed, decode_timeline, graph_segments, paint_graph, query_window

END = 1_800_000_000  # UTC-aligned minute.


def payload():
    return {"granularity": "minute", "coverage": "fields", "accounting": "completed_requests",
            "buckets": [{"start": "2027-01-15T07:56:00Z", "bucket_seconds": 60,
                         "request_count": 2, "requests_with_usage": 2,
                         "input_tokens": 120, "output_tokens": 0,
                         "requests_with_input_usage": 2, "requests_with_output_usage": 0}]}


class TimelineTests(unittest.TestCase):
    def test_requests_only_complete_aligned_buckets_with_coverage(self):
        for spec in RANGES:
            start, end, query = query_window(spec, END + 17)
            self.assertEqual(end % spec.bucket_seconds, 0)
            self.assertLessEqual(end, END + 17)
            self.assertEqual(end - start, spec.duration)
            self.assertIn("coverage=fields", query)
            self.assertLessEqual(spec.duration // spec.bucket_seconds, 365)

    def test_exact_edges_rates_and_missing_output_not_zero(self):
        series = decode_timeline(payload(), RANGES[1], END - 300, END, END + 1)
        self.assertEqual(len(series.buckets), 1)
        row = series.buckets[0]
        self.assertEqual(row.start, END - 240)
        self.assertEqual(row.end, END - 180)
        self.assertEqual(row.input_rate, 120)
        self.assertIsNone(row.output_rate)
        segments = graph_segments(series, 300, 20)
        self.assertEqual(len(segments), 1)
        self.assertEqual((segments[0].x1, segments[0].x2), (60, 120))
        self.assertEqual(segments[0].series, "input")

    def test_reported_zero_is_drawn_and_partial_is_marked(self):
        raw = payload()
        raw['buckets'][0]['requests_with_output_usage'] = 1
        series = decode_timeline(raw, RANGES[1], END - 300, END, END)
        out = next(segment for segment in graph_segments(series, 300, 20) if segment.series == 'output')
        self.assertTrue(out.partial)
        self.assertEqual(out.y, 20)

    def test_coarse_buckets_use_real_duration_for_rate(self):
        raw = payload()
        raw['granularity'] = 'hour'
        raw['buckets'][0].update(start='2027-01-15T07:00:00Z', bucket_seconds=3600, input_tokens=7200)
        start, end, _query = query_window(RANGES[4], END)
        series = decode_timeline(raw, RANGES[4], start, end, END)
        self.assertEqual(series.buckets[0].input_rate, 120)

    def test_shared_linear_scale_not_independent_series_scaling(self):
        raw = payload()
        raw['buckets'][0].update(output_tokens=60, requests_with_output_usage=2)
        series = decode_timeline(raw, RANGES[1], END - 300, END, END)
        segments = graph_segments(series, 300, 20)
        self.assertEqual([(s.series, s.y) for s in segments], [('input', 0), ('output', 10)])

    def test_unknown_coverage_or_duplicate_buckets_fail_instead_of_inventing(self):
        raw = payload()
        del raw['coverage']
        with self.assertRaises(ValueError): decode_timeline(raw, RANGES[1], END - 300, END, END)
        raw = payload(); raw['buckets'] *= 2
        with self.assertRaises(ValueError): decode_timeline(raw, RANGES[1], END - 300, END, END)

    def test_invalid_counts_timestamps_and_bounds_rejected(self):
        cases = [('requests_with_input_usage', 3), ('input_tokens', True),
                 ('output_tokens', -1), ('bucket_seconds', 0),
                 ('start', '2027-01-15T07:56:01Z'), ('start', '2027-01-15T07:56:00'),
                 ('start', '2027-01-15T08:00:00Z')]
        for field, value in cases:
            raw = payload(); raw['buckets'][0][field] = value
            with self.subTest(field=field, value=value), self.assertRaises(ValueError):
                decode_timeline(raw, RANGES[1], END - 300, END, END)

    def test_empty_history_is_gap_not_zero_samples(self):
        raw = payload(); raw['buckets'] = []
        series = decode_timeline(raw, RANGES[1], END - 300, END, END)
        self.assertEqual(graph_segments(series, 300, 20), ())

    def test_cairo_paints_actual_segments_and_no_data_state(self):
        try:
            import cairocffi as cairo
        except ImportError:
            self.skipTest('Cairo is exercised by the real Qtile CI dependency')
        series = decode_timeline(payload(), RANGES[1], END - 300, END, END)
        for selected in (series, None):
            surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, 180, 32)
            paint_graph(cairo.Context(surface), 180, 32, '5m', selected, stale=False)
            surface.flush()
            self.assertTrue(any(bytes(surface.get_data())))


class FeedTests(unittest.TestCase):
    def test_shared_rotation_pause_resume_and_no_network_on_read(self):
        feed = TimelineFeed('http://localhost:8787')
        with patch('llm_log_graph.time.monotonic', return_value=0):
            feed.changed_at = 0
        with patch('llm_log_graph.time.monotonic', return_value=5), patch.object(feed, 'refresh_once', side_effect=AssertionError('UI network')):
            self.assertEqual(feed.read()[0].label, '5m')
            feed.advance()
            self.assertEqual(feed.read()[0].label, '1h')
            self.assertTrue(feed.paused)
            feed.resume()
            self.assertFalse(feed.paused)

    def test_failure_retains_last_good_and_marks_failed(self):
        feed = TimelineFeed('http://localhost:8787')
        feed.index = 1
        with patch('llm_log_graph.time.time', return_value=END), patch('llm_log_graph.fetch_json', return_value=payload()):
            feed.refresh_once()
        self.assertIsNotNone(feed.cache[1])
        before = feed.cache[1]
        with patch('llm_log_graph.time.time', return_value=END + 30), patch('llm_log_graph.fetch_json', side_effect=ValueError('secret')):
            feed.refresh_once()
        self.assertIs(feed.cache[1], before)
        self.assertTrue(feed.failed[1])

    def test_provider_model_filters_are_encoded_not_executed(self):
        feed = TimelineFeed('http://localhost:8787', provider='zai', model='vendor/model x&y')
        feed.index = 1
        with patch('llm_log_graph.time.time', return_value=END), patch('llm_log_graph.fetch_json', return_value=payload()) as fetch:
            feed.refresh_once()
        url = fetch.call_args.args[0]
        self.assertIn('provider=zai', url)
        self.assertIn('model=vendor%2Fmodel+x%26y', url)


class LiterateTests(unittest.TestCase):
    def test_graph_org_matches_generated(self):
        org = (ROOT / 'qtile-llm-log-graph.org').read_text()
        blocks = re.findall(r'^#\+begin_src python :tangle ([^\n]+)\n(.*?)^#\+end_src', org, re.M | re.S)
        self.assertEqual(len(blocks), 2)
        for filename, content in blocks:
            self.assertEqual((ROOT / filename).read_text(), content)


class GraphWidgetTests(unittest.TestCase):
    def test_pair_then_graph_adjacency_idempotency_and_real_cairo_draw(self):
        try:
            import cairocffi as cairo
        except ImportError:
            self.skipTest('Cairo is required for the rendering contract')
        class FakeBase:
            def __init__(self, length, **config):
                self.name = config.get('name')
                self.width, self.height, self.background = length, 32, None
                self.offsetx = self.offsety = 0
            def add_callbacks(self, callbacks): self.callbacks = callbacks
            def _configure(self, qtile, bar): pass
            def finalize(self): pass
            def draw_at_default_position(self): pass
        base = types.SimpleNamespace(_Widget=FakeBase, ORIENTATION_HORIZONTAL=1)
        with patch.dict(sys.modules, {
                'libqtile': types.ModuleType('libqtile'),
                'libqtile.widget': types.SimpleNamespace(base=base),
                'notify': types.SimpleNamespace(notify=lambda *a, **k: None)}):
            import importlib
            mod = importlib.import_module('qtile_llm_log_graph')
            try:
                widgets = [types.SimpleNamespace(name=name) for name in
                           ('openrouter_graph', 'llm_quota_zai', 'llm_quota_gpt', 'clock')]
                bar = types.SimpleNamespace(widgets=widgets, background='#000000')
                config = {'screens': [types.SimpleNamespace(top=bar, bottom=None)]}
                mod.install_llm_log_graph(config)
                mod.install_llm_log_graph(config)
                self.assertEqual([w.name for w in widgets],
                                 ['openrouter_graph', 'llm_quota_zai', 'llm_quota_gpt', 'llm_log_graph', 'clock'])
                widget = widgets[3]
                surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, 180, 32)
                widget.bar = bar
                widget.drawer = types.SimpleNamespace(ctx=cairo.Context(surface), clear=lambda *_: None)
                with patch.object(widget.feed, 'acquire') as acquire, patch.object(widget.feed, 'release') as release:
                    widget._configure(None, bar)
                    widget._configure(None, bar)
                    with patch.object(widget.feed, 'refresh_once', side_effect=AssertionError('network in draw')):
                        widget.draw()
                    widget.finalize()
                    widget.finalize()
                    acquire.assert_called_once()
                    release.assert_called_once()
            finally:
                sys.modules.pop('qtile_llm_log_graph', None)


if __name__ == '__main__':
    unittest.main()
