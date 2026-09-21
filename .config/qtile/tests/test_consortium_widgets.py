import importlib
import json
from pathlib import Path
import re
import sys
import tempfile
import types
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from consortium_status import (
    ConsortiumFeed,
    decode_status,
    render_phase,
    render_runs,
)

NOW = 2_000_000_000.0


def payload(state="running", phase="develop", active_count=1):
    return {
        "schema_version": 1,
        "active_count": active_count,
        "runs": [{
            "run_id": "run-1",
            "sequence": 4,
            "state": state,
            "phase": phase,
            "worker": "rlm",
            "event_type": "phase.started" if state == "running" else f"run.{state}",
            "updated_at": "2026-09-21T03:00:00Z",
        }],
    }


class ModelTests(unittest.TestCase):
    def test_active_and_terminal_rendering(self):
        snap = decode_status(payload(), fetched_at=NOW, source_mtime=NOW)
        self.assertEqual(render_runs(snap), ("CONS 1", "active"))
        self.assertEqual(render_phase(snap), ("ADARD develop", "active"))

        done = decode_status(payload("completed", "verify", 0), fetched_at=NOW, source_mtime=NOW)
        self.assertEqual(render_runs(done), ("CONS ✓", "ok"))
        self.assertEqual(render_phase(done), ("ADARD completed", "ok"))

        failed = decode_status(payload("failed", "verify", 0), fetched_at=NOW, source_mtime=NOW)
        self.assertEqual(render_runs(failed), ("CONS !", "error"))
        self.assertEqual(render_phase(failed), ("ADARD failed", "error"))

    def test_parser_rejects_unbounded_or_inconsistent_state(self):
        bad = payload()
        bad["active_count"] = 0
        with self.assertRaises(ValueError):
            decode_status(bad, fetched_at=NOW, source_mtime=NOW)
        bad = payload()
        bad["runs"] *= 17
        with self.assertRaises(ValueError):
            decode_status(bad, fetched_at=NOW, source_mtime=NOW)

    def test_failed_refresh_retains_last_good_snapshot(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "qtile-status.json"
            path.write_text(json.dumps(payload()), encoding="utf-8")
            feed = ConsortiumFeed(path)
            feed.refresh_once()
            first, failed = feed.read()
            self.assertFalse(failed)
            self.assertEqual(first.active_count, 1)

            path.write_text("{", encoding="utf-8")
            feed.refresh_once()
            second, failed = feed.read()
            self.assertTrue(failed)
            self.assertIs(first, second)
            self.assertEqual(render_runs(second, failed), ("CONS ~1", "neutral"))

    def test_missing_file_is_explicit_unavailable(self):
        with tempfile.TemporaryDirectory() as directory:
            feed = ConsortiumFeed(Path(directory) / "missing.json")
            feed.refresh_once()
            snap, failed = feed.read()
            self.assertIsNone(snap)
            self.assertFalse(failed)
            self.assertEqual(render_runs(snap), ("CONS --", "neutral"))


class FakePoll:
    def __init__(self, text="", **config):
        self.text = text
        self.name = config.get("name", "unnamed")
        self.configured = False

    def add_callbacks(self, callbacks):
        self.callbacks = callbacks

    def _configure(self, qtile, bar):
        self.qtile = qtile
        self.bar = bar
        self.configured = True

    def update(self, text):
        self.text = text

    def finalize(self):
        self.configured = False


class WidgetTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        base = types.SimpleNamespace(InLoopPollText=FakePoll, ORIENTATION_HORIZONTAL=1)
        cls.stub = patch.dict(sys.modules, {
            "libqtile": types.ModuleType("libqtile"),
            "libqtile.widget": types.SimpleNamespace(base=base),
            "notify": types.SimpleNamespace(notify=lambda *args, **kwargs: None),
        })
        cls.stub.start()
        cls.mod = importlib.import_module("qtile_consortium")

    @classmethod
    def tearDownClass(cls):
        sys.modules.pop("qtile_consortium", None)
        cls.stub.stop()

    def test_primary_screen_only_and_subscription_adjacency(self):
        first_bar = types.SimpleNamespace(widgets=[
            types.SimpleNamespace(name="llm_quota_zai"),
            types.SimpleNamespace(name="llm_quota_gpt"),
            types.SimpleNamespace(name="clock"),
        ])
        second_bar = types.SimpleNamespace(widgets=[types.SimpleNamespace(name="clock")])
        config = {
            "screens": [
                types.SimpleNamespace(top=first_bar, bottom=None),
                types.SimpleNamespace(top=second_bar, bottom=None),
            ]
        }
        self.mod.install_consortium_widgets(config)
        self.mod.install_consortium_widgets(config)
        self.assertEqual(
            [item.name for item in first_bar.widgets],
            ["consortium_runs", "consortium_phase", "llm_quota_zai", "llm_quota_gpt", "clock"],
        )
        self.assertEqual([item.name for item in second_bar.widgets], ["clock"])

    def test_pair_shares_one_file_feed_and_render_is_memory_only(self):
        pair = self.mod.make_consortium_widgets()
        self.assertIs(pair[0].feed, pair[1].feed)
        with patch.object(pair[0].feed, "refresh_once", side_effect=AssertionError("file I/O in render")):
            self.assertIn("CONS", pair[0].poll())
            self.assertIn("ADARD", pair[1].poll())


class LiterateTests(unittest.TestCase):
    def test_org_generated_parity(self):
        org = (ROOT / "qtile-consortium.org").read_text(encoding="utf-8")
        blocks = re.findall(r"^#\+begin_src python :tangle ([^\n]+)\n(.*?)^#\+end_src", org, re.M | re.S)
        self.assertEqual(len(blocks), 2)
        for filename, code in blocks:
            self.assertEqual((ROOT / filename).read_text(encoding="utf-8"), code)

    def test_telemetry_composes_consortium_after_subscription_pair(self):
        source = (ROOT / "qtile_telemetry.py").read_text(encoding="utf-8")
        self.assertIn("install_consortium_widgets(config_globals)", source)
        self.assertLess(source.index("install_llm_log_widgets(config_globals)"),
                        source.index("install_consortium_widgets(config_globals)"))
        self.assertLess(source.index("install_consortium_widgets(config_globals)"),
                        source.index("install_llm_log_graph(config_globals)"))


if __name__ == "__main__":
    unittest.main()
