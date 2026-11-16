"""Quota model + real local HTTP; Qtile shell/lifecycle uses explicit test doubles."""
from __future__ import annotations
import importlib
import json
import os
from pathlib import Path
import re
import sys
import threading
import time
import types
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from llm_log_client import QuotaFeed, decode_quotas, render_quota, details, validate_base_url

NOW = 2_000_000_000.0

def payload(percent=31, state='ok'):
    return {'schema_version': 1, 'generated_at': NOW, 'stale_after_seconds': 180,
            'providers': [{'id': key, 'state': state, 'plan': 'pro', 'source': 'test',
                           'scope': 'codex' if key == 'gpt' else 'coding-plan',
                           'updated_at': NOW, 'windows': [
                {'id': 'codex:primary', 'meter': 'codex', 'label': '5h', 'used_percent': percent,
                 'window_seconds': 18000, 'resets_at': NOW + 600, 'state': 'ok'},
                {'id': 'codex:secondary', 'meter': 'codex', 'label': 'week', 'used_percent': 92,
                 'window_seconds': 604800, 'resets_at': NOW + 86400, 'state': 'ok'}]}
                for key in ('zai', 'gpt')]}

class ModelTests(unittest.TestCase):
    def test_labels_and_percentage_used_threshold_boundaries(self):
        for percent, expected in ((0, 'green'), (69.9, 'green'), (70, 'yellow'), (89.9, 'yellow'), (90, 'red'), (100, 'red')):
            snap = decode_quotas(payload(percent), NOW)
            for key, label in (('zai', 'z.AI'), ('gpt', 'GPT')):
                rendered, level = render_quota(snap, key, 0, NOW)
                self.assertTrue(rendered.startswith(label + ' '))
                self.assertIn(f'{percent:g}%', rendered)
                self.assertEqual(level, expected)

    def test_invalid_numbers_cannot_turn_green(self):
        for value in (None, True, float('nan'), float('inf'), -1, 101, '40'):
            snap = decode_quotas(payload(value), NOW)
            rendered, level = render_quota(snap, 'gpt', 0, NOW)
            self.assertIn('?', rendered)
            self.assertEqual(level, 'neutral')

    def test_provider_and_snapshot_staleness(self):
        for state in ('stale', 'disabled', 'unavailable'):
            snap = decode_quotas(payload(state=state), NOW)
            rendered, level = render_quota(snap, 'zai', 0, NOW)
            self.assertEqual(level, 'neutral')
            self.assertIn(state, rendered)
        snap = decode_quotas(payload(), NOW)
        self.assertIn('stale', render_quota(snap, 'gpt', 0, NOW + 181)[0])
        self.assertIn('stale', render_quota(snap, 'gpt', 0, NOW, offline=True)[0])

    def test_reset_expiry_does_not_invent_zero(self):
        snap = decode_quotas(payload(), NOW + 600)
        rendered, level = render_quota(snap, 'gpt', 0, NOW + 600)
        self.assertIn('expired', rendered)
        self.assertNotIn('0%', rendered)
        self.assertEqual(level, 'neutral')

    def test_missing_snapshot_or_provider_not_zero(self):
        self.assertEqual(render_quota(None, 'zai', 0, NOW), ('z.AI unavailable', 'neutral'))
        raw = payload(); raw['providers'] = []
        self.assertEqual(render_quota(decode_quotas(raw, NOW), 'gpt', 0, NOW), ('GPT unavailable', 'neutral'))

    def test_plan_metadata_and_meter_scope_in_details(self):
        raw = payload(); raw['providers'][1]['plan'] = 'future-plan'
        result = details(decode_quotas(raw, NOW), 'gpt', NOW)
        self.assertIn('future-plan', result)
        self.assertIn('codex', result)
        self.assertIn('5h', result)
        self.assertIn('week', result)
        self.assertIn('not all ChatGPT', result)

    def test_malformed_and_oversized_shapes_rejected(self):
        for bad in ([], {}, {'schema_version': True}, {**payload(), 'providers': [{}] * 65}):
            with self.assertRaises(ValueError): decode_quotas(bad, NOW)
        raw = payload(); raw['providers'][0]['windows'] *= 40
        with self.assertRaises(ValueError): decode_quotas(raw, NOW)

    def test_only_local_no_auth_urls(self):
        for good in ('http://127.0.0.1:8787', 'http://[::1]:8787', 'http://localhost:8787/'):
            self.assertTrue(validate_base_url(good).startswith('http://'))
        for bad in ('http://evil.invalid', 'file:///tmp/key', 'http://user:secret@localhost',
                    'http://localhost/?token=secret', 'http://127.0.0.1:8787/path', 'http://localhost#fragment'):
            with self.assertRaises(ValueError): validate_base_url(bad)

class TransportTests(unittest.TestCase):
    def test_real_http_bound_and_invalid_response_retains_stale_snapshot(self):
        state = {'body': json.dumps(payload()).encode()}
        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):
                self.send_response(200); self.end_headers(); self.wfile.write(state['body'])
            def log_message(self, *_args): pass
        server = ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        worker = threading.Thread(target=server.serve_forever, daemon=True); worker.start()
        try:
            feed = QuotaFeed(f'http://127.0.0.1:{server.server_port}')
            feed.refresh_once()
            snap, failed = feed.read()
            self.assertFalse(failed); self.assertEqual(snap.providers[1].windows[0].used_percent, 31)
            state['body'] = b'private error body'
            feed.refresh_once()
            snap2, failed = feed.read()
            self.assertTrue(failed); self.assertIs(snap, snap2)
            state['body'] = b'x' * 262145
            feed.refresh_once(); self.assertTrue(feed.read()[1])
        finally:
            server.shutdown(); server.server_close(); worker.join(2)

# No installed Qtile in this environment. These doubles test our integration,
# not Cairo layout, a real WM session, or actual Qtile lifecycle internals.
class FakePoll:
    def __init__(self, text='', **config):
        self.text = text
        self.name = config.get('name', 'unnamed')
        self.configured = False
    def add_callbacks(self, callbacks): self.callbacks = callbacks
    def _configure(self, qtile, bar): self.qtile = qtile; self.bar = bar; self.configured = True
    def update(self, text): self.text = text
    def finalize(self): self.configured = False

class WidgetTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.base = types.SimpleNamespace(InLoopPollText=FakePoll, ORIENTATION_HORIZONTAL=1)
        cls.stub = patch.dict(sys.modules, {
            'libqtile': types.ModuleType('libqtile'),
            'libqtile.widget': types.SimpleNamespace(base=cls.base),
            'notify': types.SimpleNamespace(notify=lambda *a, **kw: None)})
        cls.stub.start()
        cls.mod = importlib.import_module('qtile_llm_log')
    @classmethod
    def tearDownClass(cls):
        sys.modules.pop('qtile_llm_log', None); cls.stub.stop()

    def test_widgets_adjacent_and_install_idempotent_per_bar(self):
        clock = types.SimpleNamespace(name='clock')
        bar = types.SimpleNamespace(widgets=[clock])
        screen = types.SimpleNamespace(top=bar, bottom=None)
        config = {'screens': [screen]}
        self.mod.install_llm_log_widgets(config)
        self.mod.install_llm_log_widgets(config)
        self.assertEqual([w.provider_id for w in bar.widgets if isinstance(w, self.mod.LLMQuota)], ['zai', 'gpt'])
        self.assertIs(bar.widgets[2], clock)

    def test_rotation_pause_resume_and_shared_feed(self):
        pair = self.mod.make_quota_widgets()
        self.assertIs(pair[0].feed, pair[1].feed)
        pair[0].feed.publish(decode_quotas(payload(), NOW))
        widget = pair[0]
        widget.changed_at = 100
        with patch.object(self.mod.time, 'monotonic', return_value=105), patch.object(self.mod.time, 'time', return_value=NOW):
            self.assertIn('week', widget.poll())
            widget.advance(); self.assertTrue(widget.paused)
            self.assertIn('5h', widget.poll())
            widget.resume(); self.assertFalse(widget.paused)

    def test_pango_injection_is_escaped(self):
        raw = payload(); raw['providers'][1]['windows'][0]['label'] = '<b>5h</b>'
        widget = self.mod.make_quota_widgets()[1]
        widget.feed.publish(decode_quotas(raw, NOW))
        with patch.object(self.mod.time, 'time', return_value=NOW):
            rendered = widget.poll()
        self.assertNotIn('<b>', rendered); self.assertIn('&lt;b&gt;', rendered)

    def test_lifecycle_reuses_feed_and_releases_once(self):
        widget = self.mod.make_quota_widgets()[0]
        with patch.object(widget.feed, 'acquire') as acquire, patch.object(widget.feed, 'release') as release:
            widget._configure(None, None)
            widget._configure(None, None)
            widget.finalize()
            widget.finalize()
            acquire.assert_called_once()
            release.assert_called_once()

    def test_render_never_fetches_or_starts_subprocess(self):
        widget = self.mod.make_quota_widgets()[0]
        with patch.object(widget.feed, 'refresh_once', side_effect=AssertionError('network in render')):
            widget.poll()

class LiterateTests(unittest.TestCase):
    def test_existing_telemetry_is_integrated_and_tangled(self):
        import ast
        org = (ROOT / 'qtile-telemetry.org').read_text()
        code = re.search(r'^#\+begin_src python :tangle qtile_telemetry.py\n(.*?)^#\+end_src', org, re.M | re.S).group(1)
        self.assertEqual(code, (ROOT / 'qtile_telemetry.py').read_text())
        tree = ast.parse(code)
        installer = next(node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name == 'install_telemetry')
        calls = [node.value.func.id for node in installer.body if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Name)]
        self.assertLess(calls.index('install_openrouter_widget'), calls.index('install_llm_log_widgets'))

    def test_new_module_org_generated_parity(self):
        org = (ROOT / 'qtile-llm-log.org').read_text()
        blocks = re.findall(r'^#\+begin_src python :tangle ([^\n]+)\n(.*?)^#\+end_src', org, re.M | re.S)
        self.assertEqual(len(blocks), 2)
        for filename, code in blocks:
            self.assertEqual((ROOT / filename).read_text(), code)

if __name__ == '__main__': unittest.main()
