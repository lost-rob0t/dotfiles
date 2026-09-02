#!/usr/bin/env python3
"""Regression tests for declarative Qtile workflow lifecycle actions."""

from __future__ import annotations

import importlib.util
import json
import sys
import types
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "qtile_workflows.py"
WORKFLOWS = ROOT / "workflows.json"


class Window:
    def __init__(self):
        self.killed = 0

    def kill(self):
        self.killed += 1


class Layout:
    def __init__(self, name):
        self.name = name


class Group:
    def __init__(self, name, windows=()):
        self.name = name
        self.windows = list(windows)
        self.layout = Layout("monadtall")
        self.setlayouts = []

    def setlayout(self, name):
        self.setlayouts.append(name)
        self.layout.name = name


class Qtile:
    def __init__(self, groups):
        self.groups = groups
        self.groups_map = {group.name: group for group in groups}


class WorkflowRuntimeTests(unittest.TestCase):
    def setUp(self):
        self.base_calls = []
        fake = types.ModuleType("qtile_control")

        def base_apply(qtile, workflow, config_globals):
            self.base_calls.append((qtile, workflow, config_globals))

        fake.apply_workflow = base_apply
        fake._notify = mock.Mock()
        self.fake_control = fake
        self.previous_control = sys.modules.get("qtile_control")
        sys.modules["qtile_control"] = fake

        spec = importlib.util.spec_from_file_location("qtile_workflows_under_test", SOURCE)
        self.module = importlib.util.module_from_spec(spec)
        assert spec.loader is not None
        spec.loader.exec_module(self.module)

    def tearDown(self):
        if self.previous_control is None:
            sys.modules.pop("qtile_control", None)
        else:
            sys.modules["qtile_control"] = self.previous_control

    def config(self, qtile):
        calls = {"buttons": 0, "layouts": 0}

        def update_buttons():
            calls["buttons"] += 1

        def update_group_layout(group):
            desired = "max" if group.name == "1" else "monadtall"
            if group.layout.name != desired:
                group.setlayout(desired)

        def update_auto_layouts(runtime):
            calls["layouts"] += 1
            for group in runtime.groups:
                config["update_group_layout"](group)

        config = {
            "auto_group_mode": False,
            "update_auto_group_buttons": update_buttons,
            "update_group_layout": update_group_layout,
            "update_auto_layouts": update_auto_layouts,
        }
        return config, calls

    def test_import_installs_apply_workflow_wrapper(self):
        self.assertIs(self.fake_control.apply_workflow, self.module.apply_workflow)

    def test_starintel_actions_close_windows_enable_auto_and_hold_group_three_max(self):
        windows = [Window(), Window(), Window()]
        groups = [Group("1", windows[:1]), Group("2", windows[1:2]), Group("3", windows[2:])]
        qtile = Qtile(groups)
        config, calls = self.config(qtile)
        workflow = {
            "auto_group": True,
            "auto_mode": True,
            "close_all": True,
            "layouts": {"3": "max"},
            "launch": [{"argv": ["true"]}],
        }

        with mock.patch.object(self.module, "_launch_async") as launch:
            self.module.apply_workflow(qtile, workflow, config)

        self.assertEqual([window.killed for window in windows], [1, 1, 1])
        self.assertTrue(config["auto_group_mode"])
        self.assertEqual(calls["buttons"], 1)
        self.assertEqual(groups[2].layout.name, "max")
        self.assertGreaterEqual(calls["layouts"], 2)
        self.assertEqual(len(self.base_calls), 1)
        launch.assert_called_once_with(workflow["launch"])

        config["update_group_layout"](groups[2])
        self.assertEqual(groups[2].layout.name, "max")

    def test_next_workflow_clears_layout_override(self):
        groups = [Group("1"), Group("2"), Group("3")]
        qtile = Qtile(groups)
        config, _calls = self.config(qtile)

        with mock.patch.object(self.module, "_launch_async"):
            self.module.apply_workflow(qtile, {"layouts": {"3": "max"}}, config)
            self.assertEqual(groups[2].layout.name, "max")
            self.module.apply_workflow(qtile, {}, config)

        self.assertEqual(groups[2].layout.name, "monadtall")

    def test_workflow_definition_has_requested_starintel_slice(self):
        payload = json.loads(WORKFLOWS.read_text(encoding="utf-8"))
        starintel = payload["starintel"]
        self.assertTrue(starintel["close_all"])
        self.assertTrue(starintel["auto_mode"])
        self.assertEqual(starintel["layouts"], {"3": "max"})
        self.assertEqual(
            starintel["screens"],
            {"left": "1", "center": "2", "right": "3", "aux": "8"},
        )
        launches = [entry["argv"] for entry in starintel["launch"]]
        self.assertEqual(launches[0], ["brave", "https://git.starintel.actor"])
        self.assertEqual(launches[1][-1], "~/starintel")
        self.assertEqual(launches[2][-1], "opencode")
        self.assertIn("OpenCode", launches[2])


if __name__ == "__main__":
    unittest.main()
