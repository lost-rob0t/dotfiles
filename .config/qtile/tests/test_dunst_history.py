#!/usr/bin/env python3
"""Behavioral tests for the shared Dunst history adapter."""

from __future__ import annotations

import importlib.util
from unittest import mock
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "scripts" / "dunst_history.py"
SPEC = importlib.util.spec_from_file_location("dunst_history", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class DunstHistoryTests(unittest.TestCase):
    def test_normalizes_urgency_and_keeps_all_entries(self):
        payload = {
            "data": [[
                {
                    "id": {"data": 1},
                    "appname": {"data": "Build"},
                    "summary": {"data": "Finished"},
                    "body": {"data": "All green"},
                    "urgency": {"data": "critical"},
                    "timestamp": {"data": "17:54"},
                },
                {
                    "id": {"data": 2},
                    "appname": {"data": "Chat"},
                    "summary": {"data": "Message"},
                    "body": {"data": "Hello"},
                    "urgency": {"data": "low"},
                },
            ]],
        }
        entries = MODULE.history_entries(payload)
        self.assertEqual(len(entries), 2)
        self.assertEqual(entries[0]["urgency"], "critical")
        self.assertEqual(entries[1]["urgency"], "low")
        self.assertEqual(entries[0]["timestamp"], "17:54")

    def test_snapshot_has_no_entry_limit(self):
        entries = [{"id": i} for i in range(143)]
        with mock.patch.object(MODULE, "_history_with_error", return_value=(entries, None)), mock.patch.object(
            MODULE, "_paused_with_error", return_value=(False, None)
        ):
            self.assertEqual(len(MODULE.snapshot()["entries"]), 143)

    def test_snapshot_surfaces_dunst_failures(self):
        with mock.patch.object(
            MODULE, "_history_with_error", return_value=([], "dunstctl history failed")
        ), mock.patch.object(MODULE, "_paused_with_error", return_value=(False, None)):
            self.assertEqual(MODULE.snapshot()["error"], "dunstctl history failed")


if __name__ == "__main__":
    unittest.main()
