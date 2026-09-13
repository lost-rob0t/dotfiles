"""Quota widgets consume llm-log observations, not local token estimates."""
from __future__ import annotations

import importlib.util
import json
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]


class FakePoll:
    def __init__(self, text='', **config):
        self._config = config
        self.text = text
        for key, value in config.items():
            setattr(self, key, value)

    def add_defaults(self, defaults):
        for key, value, _ in defaults:
            setattr(self, key, self._config.get(key, value))

    def add_callbacks(self, callbacks):
        self.callbacks = callbacks


def load_module():
    base = types.SimpleNamespace(BackgroundPoll=FakePoll, ORIENTATION_HORIZONTAL=1)
    widget = types.ModuleType('libqtile.widget')
    widget.base = base
    modules = {'libqtile': types.ModuleType('libqtile'), 'libqtile.widget': widget}
    spec = importlib.util.spec_from_file_location('quota_widgets_under_test', ROOT / 'qtile_quotas.py')
    module = importlib.util.module_from_spec(spec)
    with patch.dict(sys.modules, modules):
        spec.loader.exec_module(module)
    return module


class QuotaWidgetTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_module()

    def provider(self, percent=42):
        return {'plan': 'pro', 'scope': 'codex', 'status': 'ok', 'observed_at': 1000,
                'windows': [{'id': 'codex:primary', 'meter': 'codex', 'duration_seconds': 18000,
                             'used_percent': percent, 'resets_at': 2000},
                            {'id': 'codex:secondary', 'meter': 'codex', 'duration_seconds': 604800,
                             'used_percent': 78, 'resets_at': 8000}]}

    def test_actual_threshold_boundaries(self):
        for value, color in [(0, '#50fa7b'), (69.9, '#50fa7b'), (70, '#f1fa8c'),
                             (89.9, '#f1fa8c'), (90, '#ff5555'), (100, '#ff5555')]:
            with self.subTest(value=value):
                rendered = self.module.render_quota('GPT', self.provider(value), 0, 1001)
                self.assertIn(color, rendered)
                self.assertIn('GPT 5h', rendered)

    def test_week_rotation_is_provider_duration_not_assumed_slot(self):
        self.assertIn('GPT wk 78%', self.module.render_quota('GPT', self.provider(), 1, 1001))
        item = self.provider()
        item['windows'][1]['duration_seconds'] = 86400
        self.assertIn('GPT 1d 78%', self.module.render_quota('GPT', item, 1, 1001))

    def test_stale_and_expired_never_look_healthy(self):
        for now in [1300, 2001]:
            value = self.module.render_quota('GPT', self.provider(), 0, now)
            self.assertIn('#888888', value)
            self.assertIn('~', value)
        item = self.provider()
        item['status'] = 'auth-required'
        self.assertIn('#888888', self.module.render_quota('GPT', item, 0, 1001))

    def test_invalid_usage_never_becomes_zero(self):
        for value in [None, True, -2, 101, float('nan'), '12']:
            rendered = self.module.render_quota('z.AI', self.provider(value), 0, 1001)
            self.assertIn('--', rendered)
            self.assertIn('#888888', rendered)

    def test_missing_provider_and_windows_are_visible(self):
        self.assertIn('z.AI --', self.module.render_quota('z.AI', {}, 0, 1001))
        self.assertIn('GPT --', self.module.render_quota('GPT', {'windows': []}, 0, 1001))

    def test_malformed_markup_and_extra_meters_are_bounded(self):
        item = self.provider()
        item['windows'][1]['meter'] = '<span foreground="red">attack</span>' * 100
        rendered = self.module.render_quota('GPT', item, 1, 1001)
        self.assertLess(len(rendered), 160)
        self.assertNotIn('foreground="red"', rendered)

    def test_cycle_pause_and_resume(self):
        widget = self.module.QuotaWidget('gpt', 'GPT')
        widget._started = 100
        with patch.object(self.module.time, 'monotonic', return_value=106):
            self.assertEqual(widget.rotation_index(), 1)
            widget.cycle()
            self.assertEqual(widget.rotation_index(), 2)
        with patch.object(self.module.time, 'monotonic', return_value=125):
            self.assertEqual(widget.rotation_index(), 2)
            widget.resume()
        with patch.object(self.module.time, 'monotonic', return_value=130):
            self.assertEqual(widget.rotation_index(), 3)

    def test_cache_is_bounded_and_invalid_json_is_unavailable(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'quotas.json'
            self.assertEqual(self.module.read_snapshot(path), {})
            for raw in ['[1,2]', '{bad', '{"schema_version":2}', ' ' * 65537]:
                path.write_text(raw)
                self.assertEqual(self.module.read_snapshot(path), {})
            payload = {'schema_version': 1, 'providers': {'gpt': self.provider()}}
            path.write_text(json.dumps(payload))
            self.assertEqual(self.module.read_snapshot(path), payload)

    def test_source_and_generated_are_exact(self):
        org = (ROOT / 'qtile-quotas.org').read_text()
        code = org.split('#+begin_src python\n', 1)[1].split('#+end_src', 1)[0]
        self.assertEqual(code, (ROOT / 'qtile_quotas.py').read_text())
        source = (ROOT / 'qtile-openrouter.org').read_text()
        self.assertIn('*quota_widgets(background=background)', source)
        self.assertIn('*quota_widgets(background=background)', (ROOT / 'qtile_openrouter.py').read_text())

    def test_factory_creates_exact_two_labels(self):
        widgets = self.module.quota_widgets(background='#000000')
        self.assertEqual([widget.label for widget in widgets], ['z.AI', 'GPT'])


if __name__ == '__main__':
    unittest.main()
