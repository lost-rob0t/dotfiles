#!/usr/bin/env python3
"""Behavioral tests for the Qtile Dunst history helper."""

from __future__ import annotations

import importlib.util
import unittest
from pathlib import Path
from unittest import mock

SOURCE = Path(__file__).resolve().parents[1] / "scripts" / "dunst_menu.py"
SPEC = importlib.util.spec_from_file_location("dunst_menu", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class DunstMenuTests(unittest.TestCase):
    def test_flattens_typed_dunst_history(self):
        payload = {
            "type": "aa{sv}",
            "data": [[
                {
                    "id": {"type": "i", "data": 42},
                    "appname": {"type": "s", "data": "Emacs"},
                    "summary": {"type": "s", "data": "Build done"},
                    "body": {"type": "s", "data": "Everything passed"},
                }
            ]],
        }
        self.assertEqual(
            MODULE.history_entries(payload),
            [{
                "id": 42,
                "app": "Emacs",
                "summary": "Build done",
                "body": "Everything passed",
                "urgency": "normal",
                "timestamp": "",
                "category": "",
            }],
        )

    def test_menu_contains_dnd_clear_and_every_history_entry(self):
        entries = [
            {"id": index, "app": "App", "summary": f"Message {index}", "body": ""}
            for index in range(1, 31)
        ]
        rows = MODULE.menu_rows(entries, paused=True)
        self.assertEqual(len(rows), 32)
        self.assertIn("DND: ON", rows[0])
        self.assertIn("Clear notification history", rows[1])
        self.assertTrue(rows[-1].startswith("#30"))

    def test_status_marks_paused_and_running(self):
        count = mock.Mock(returncode=0, stdout="27\n")
        with mock.patch.object(MODULE, "_run", return_value=count), mock.patch.object(MODULE, "_paused", return_value=False):
            self.assertEqual(MODULE.status_text(), " 27")
        with mock.patch.object(MODULE, "_run", return_value=count), mock.patch.object(MODULE, "_paused", return_value=True):
            self.assertEqual(MODULE.status_text(), " 27")

    def test_selection_replays_exact_history_id(self):
        entries = [{"id": 91, "app": "A", "summary": "S", "body": "B"}]
        commands = []

        def fake_run(command, **_kwargs):
            commands.append(command)
            return mock.Mock(returncode=0, stdout="false\n")

        with mock.patch.object(MODULE, "_history", return_value=entries), \
             mock.patch.object(MODULE, "_paused", return_value=False), \
             mock.patch.object(MODULE, "_dmenu", return_value="#91  A: S — B"), \
             mock.patch.object(MODULE, "_run", side_effect=fake_run):
            self.assertEqual(MODULE.show_menu(), 0)
        self.assertIn(["dunstctl", "history-pop", "91"], commands)

    def test_clear_and_dnd_actions_use_dunstctl(self):
        for selection, expected in (
            (" DND: OFF  — toggle", ["dunstctl", "set-paused", "toggle"]),
            ("󰆴 Clear notification history", ["dunstctl", "history-clear"]),
        ):
            commands = []

            def fake_run(command, **_kwargs):
                commands.append(command)
                return mock.Mock(returncode=0, stdout="")

            with mock.patch.object(MODULE, "_history", return_value=[]), \
                 mock.patch.object(MODULE, "_paused", return_value=False), \
                 mock.patch.object(MODULE, "_dmenu", return_value=selection), \
                 mock.patch.object(MODULE, "_run", side_effect=fake_run):
                self.assertEqual(MODULE.show_menu(), 0)
            self.assertIn(expected, commands)


if __name__ == "__main__":
    unittest.main()
