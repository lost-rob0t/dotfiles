#!/usr/bin/env python3
"""Behavioral tests for persistent notification UI settings."""

from __future__ import annotations

import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "ui_settings.py"
SPEC = importlib.util.spec_from_file_location("ui_settings", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class UiSettingsTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.path = Path(self.temp.name) / "ui-settings.json"

    def tearDown(self):
        self.temp.cleanup()

    def test_missing_or_invalid_json_uses_dmenu_default(self):
        self.assertEqual(MODULE.get_notification_ui(self.path), "dmenu")
        self.path.write_text("not json", encoding="utf-8")
        self.assertEqual(MODULE.get_notification_ui(self.path), "dmenu")
        self.path.write_text(json.dumps({"notification_ui": "x11"}), encoding="utf-8")
        self.assertEqual(MODULE.get_notification_ui(self.path), "dmenu")

    def test_dmenu_and_emacs_values_persist(self):
        self.assertEqual(MODULE.set_notification_ui("emacs", self.path), "emacs")
        self.assertEqual(MODULE.get_notification_ui(self.path), "emacs")
        self.assertEqual(MODULE.set_notification_ui("dmenu", self.path), "dmenu")
        self.assertEqual(MODULE.get_notification_ui(self.path), "dmenu")

    def test_invalid_value_is_rejected(self):
        with self.assertRaises(ValueError):
            MODULE.set_notification_ui("web", self.path)

    def test_atomic_save_leaves_only_final_settings_file(self):
        MODULE.save_ui_settings({"notification_ui": "emacs"}, self.path)
        self.assertEqual(json.loads(self.path.read_text(encoding="utf-8"))["notification_ui"], "emacs")
        self.assertEqual(list(self.path.parent.glob(".*.ui-settings.json.*")), [])


if __name__ == "__main__":
    unittest.main()
