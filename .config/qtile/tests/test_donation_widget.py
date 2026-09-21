from __future__ import annotations

import importlib
import json
from pathlib import Path
import re
import sys
import types
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))


class FakeThreadPoolText:
    def __init__(self, text="", **config):
        self.text = text
        self.name = config.get("name", "unnamed")
        self.callbacks = {}

    def add_callbacks(self, callbacks):
        self.callbacks.update(callbacks)


class DonationWidgetTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        base = types.SimpleNamespace(
            ThreadPoolText=FakeThreadPoolText,
            ORIENTATION_HORIZONTAL=1,
        )
        cls.notifications = []
        cls.stub = patch.dict(
            sys.modules,
            {
                "libqtile": types.ModuleType("libqtile"),
                "libqtile.widget": types.SimpleNamespace(base=base),
                "notify": types.SimpleNamespace(
                    notify=lambda *args, **kwargs: cls.notifications.append(args)
                ),
            },
        )
        cls.stub.start()
        cls.mod = importlib.import_module("qtile_donations")

    @classmethod
    def tearDownClass(cls):
        sys.modules.pop("qtile_donations", None)
        cls.stub.stop()

    def result(self, raised="125.50", goal="1000.00"):
        payload = {
            "version": "ZARA-DONATIONS/1",
            "summary": {
                "raised_usd": raised,
                "goal_usd": goal,
                "remaining_usd": "874.50",
            },
            "campaigns": [
                {
                    "id": "infra",
                    "title": "Infrastructure",
                    "raised_usd": raised,
                    "goal_usd": goal,
                }
            ],
        }
        return types.SimpleNamespace(
            returncode=0,
            stdout=json.dumps(payload),
            stderr="",
        )

    def test_poll_uses_canonical_zara_json_cli(self):
        widget = self.mod.DonationTotals()
        with patch.object(self.mod.subprocess, "run", return_value=self.result()) as run:
            self.assertEqual(widget.poll(), "♥ $125.50/$1k")
        run.assert_called_once_with(
            ["zara", "--donations-json"],
            check=False,
            capture_output=True,
            text=True,
            timeout=3,
        )

    def test_failure_is_visible_not_zero(self):
        widget = self.mod.DonationTotals()
        failed = types.SimpleNamespace(returncode=2, stdout="", stderr="bad config")
        with patch.object(self.mod.subprocess, "run", return_value=failed):
            self.assertEqual(widget.poll(), "♥ —")
        self.assertIn("bad config", widget._detail_text)

    def test_install_is_idempotent_and_before_clock(self):
        clock = types.SimpleNamespace(name="clock")
        bar = types.SimpleNamespace(widgets=[clock])
        screen = types.SimpleNamespace(top=bar, bottom=None)
        config = {"screens": [screen]}
        self.mod.install_donation_widget(config)
        self.mod.install_donation_widget(config)
        names = [getattr(item, "name", "") for item in bar.widgets]
        self.assertEqual(names, ["zara_donations", "clock"])

    def test_click_uses_cached_details_without_another_cli_call(self):
        widget = self.mod.DonationTotals()
        with patch.object(self.mod.subprocess, "run", return_value=self.result()):
            widget.poll()
        with patch.object(
            self.mod.subprocess,
            "run",
            side_effect=AssertionError("click must not query"),
        ):
            widget.show_details()
        self.assertIn("Infrastructure", self.notifications[-1][1])

    def test_literate_source_matches_generated_module(self):
        org = (ROOT / "qtile-donations.org").read_text()
        match = re.search(
            r"^#\\+begin_src python\\n(.*?)^#\\+end_src",
            org,
            re.M | re.S,
        )
        self.assertIsNotNone(match)
        self.assertEqual(match.group(1), (ROOT / "qtile_donations.py").read_text())


if __name__ == "__main__":
    unittest.main()
