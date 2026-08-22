#!/usr/bin/env python3
"""Topology and config regression tests for Qtile desktop control."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from dataclasses import dataclass
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile_control.py"
SPEC = importlib.util.spec_from_file_location("qtile_control", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)
SOURCE_TEXT = SOURCE.read_text(encoding="utf-8")


@dataclass
class Rect:
    x: int
    width: int = 1920


@dataclass
class Output:
    name: str
    rect: Rect


def outputs(count: int):
    return [Output(str(index), Rect(index * 1920)) for index in range(count)]


class QtileControlTests(unittest.TestCase):
    def test_n1_is_center(self):
        self.assertEqual(MODULE.screen_roles(outputs(1)), ["center"])

    def test_n2_is_left_center(self):
        self.assertEqual(MODULE.screen_roles(outputs(2)), ["left", "center"])

    def test_n3_is_left_center_right(self):
        self.assertEqual(MODULE.screen_roles(outputs(3)), ["left", "center", "right"])

    def test_n4_has_all_primary_roles_and_one_aux(self):
        roles = MODULE.screen_roles(outputs(4))
        self.assertEqual(set(roles), {"left", "center", "right", "aux"})
        self.assertEqual(roles[0], "left")
        self.assertEqual(roles[-1], "right")

    def test_roles_follow_geometry_not_enumeration_order(self):
        shuffled = [outputs(3)[2], outputs(3)[0], outputs(3)[1]]
        self.assertEqual(MODULE.screen_roles(shuffled), ["right", "left", "center"])

    def test_primary_monitor_accents_are_distinct(self):
        accents = [MODULE.role_accent(role) for role in ("left", "center", "right", "aux")]
        self.assertEqual(len(accents), len(set(accents)))

    def test_private_env_parser_ignores_comments_and_expands_values(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "private.env"
            path.write_text("# secret config\nAGENT_ZERO_HOST=http://127.0.0.1:5080\nEMPTY=\n", encoding="utf-8")
            values = MODULE.parse_private_env(path)
        self.assertEqual(values["AGENT_ZERO_HOST"], "http://127.0.0.1:5080")
        self.assertEqual(values["EMPTY"], "")

    def test_workflow_loader_falls_back_on_invalid_json(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "workflows.json"
            path.write_text("{nope", encoding="utf-8")
            self.assertEqual(MODULE.load_workflows(path), MODULE.DEFAULT_WORKFLOWS)

    def test_runtime_bar_source_has_no_weather(self):
        self.assertNotIn("wttr.in", SOURCE_TEXT)
        self.assertNotIn("OpenWeather", SOURCE_TEXT)

    def test_mpris_is_scoped_to_right_branch(self):
        right = SOURCE_TEXT.index('elif role == "right":')
        mpris = SOURCE_TEXT.index("widget.Mpris2", right)
        self.assertGreater(mpris, right)
        self.assertEqual(SOURCE_TEXT.count("widget.Mpris2"), 1)

    def test_center_is_only_systray_role(self):
        self.assertEqual(SOURCE_TEXT.count("widget.Systray"), 1)
        center = SOURCE_TEXT.index('if role == "center":')
        tray = SOURCE_TEXT.index("widget.Systray", center)
        left = SOURCE_TEXT.index('elif role == "left":')
        self.assertLess(tray, left)

    def test_org_poll_uses_async_genpollcommand(self):
        self.assertIn("widget.GenPollCommand", SOURCE_TEXT)
        self.assertIn('name="org_clocked_task"', SOURCE_TEXT)

    def test_agent_zero_is_an_emacs_scratchpad(self):
        self.assertIn('"agent-zero"', SOURCE_TEXT)
        self.assertIn('"qtile-agent-zero"', SOURCE_TEXT)
        self.assertIn("qtile-agent-zero-open", SOURCE_TEXT)


if __name__ == "__main__":
    unittest.main()
