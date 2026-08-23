#!/usr/bin/env python3
"""Topology and config regression tests for Qtile desktop control."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from dataclasses import dataclass, field
from datetime import datetime
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


@dataclass
class Screen:
    x: int
    width: int = 1920


@dataclass
class Group:
    name: str
    label: str
    windows: list = field(default_factory=list)
    screen: object | None = None


def outputs(count: int):
    return [Output(str(index), Rect(index * 1920)) for index in range(count)]


class QtileControlTests(unittest.TestCase):
    def setUp(self):
        MODULE._group_owner_roles.clear()

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

    def test_empty_groups_are_not_visible(self):
        groups = [
            Group("1", "one", [object()]),
            Group("2", "two", []),
            Group("3", "", [object()]),
        ]
        self.assertEqual(
            [group.name for group in MODULE.visible_window_groups(groups)],
            ["1"],
        )

    def test_group_owner_tracks_screen_then_survives_hidden_group(self):
        screens = [Screen(0), Screen(1920), Screen(3840)]
        group = Group("1", "one", [object()], screens[0])
        self.assertEqual(MODULE.group_owner_role(group, screens), "left")
        group.screen = None
        self.assertEqual(MODULE.group_owner_role(group, screens), "left")

    def test_group_owner_is_cleared_when_group_becomes_empty(self):
        screens = [Screen(0), Screen(1920), Screen(3840)]
        group = Group("1", "one", [object()], screens[2])
        self.assertEqual(MODULE.group_owner_role(group, screens), "right")
        group.windows.clear()
        self.assertIsNone(MODULE.group_owner_role(group, screens))
        self.assertNotIn("1", MODULE._group_owner_roles)

    def test_private_env_parser_ignores_comments_and_expands_values(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "private.env"
            path.write_text(
                "# private config\nAGENT_ZERO_HOST=http://127.0.0.1:5080\nEMPTY=\n",
                encoding="utf-8",
            )
            values = MODULE.parse_private_env(path)
        self.assertEqual(values["AGENT_ZERO_HOST"], "http://127.0.0.1:5080")
        self.assertEqual(values["EMPTY"], "")

    def test_workflow_loader_falls_back_on_invalid_json(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "workflows.json"
            path.write_text("{nope", encoding="utf-8")
            self.assertEqual(MODULE.load_workflows(path), MODULE.DEFAULT_WORKFLOWS)

    def test_named_outrun_palette_has_extensions(self):
        colors = [[str(index)] * 2 for index in range(10)]
        palette = MODULE.outrun_palette(colors)
        self.assertEqual(palette["cyan"], "4")
        self.assertIn("electric_blue", palette)
        self.assertIn("violet", palette)
        self.assertIn("yellow", palette)

    def test_right_role_contains_market_weather_and_mpris(self):
        right = SOURCE_TEXT.index('elif role == "right":')
        self.assertGreater(SOURCE_TEXT.index("_market_widgets(config_globals)", right), right)
        self.assertGreater(SOURCE_TEXT.index("_weather_widget(config_globals)", right), right)
        self.assertGreater(SOURCE_TEXT.index("widget.Mpris2", right), right)
        self.assertEqual(SOURCE_TEXT.count("widget.Mpris2"), 1)

    def test_center_has_one_combined_network_graph(self):
        self.assertIn("from qtile_net_io import NetIOGraph", SOURCE_TEXT)
        self.assertEqual(SOURCE_TEXT.count("NetIOGraph("), 1)
        self.assertNotIn("widget.NetGraph(", SOURCE_TEXT)
        self.assertIn('name="combined_network_io"', SOURCE_TEXT)
        self.assertIn('download_color=palette["electric_blue"]', SOURCE_TEXT)
        self.assertIn('upload_color=palette["pink"]', SOURCE_TEXT)
        self.assertIn('format="↓{down}{down_suffix} ↑{up}{up_suffix}"', SOURCE_TEXT)
        self.assertIn('foreground="{palette[\"electric_blue\"]}">↓', SOURCE_TEXT)
        self.assertIn('foreground="{palette[\"pink\"]}">↑', SOURCE_TEXT)

    def test_center_restores_cpu_memory_system_graphs(self):
        center = SOURCE_TEXT.index('if role == "center":')
        left = SOURCE_TEXT.index('elif role == "left":')
        center_text = SOURCE_TEXT[center:left]
        self.assertIn("_system_telemetry(config_globals)", center_text)
        self.assertIn("widget.CPUGraph", SOURCE_TEXT)
        self.assertIn("widget.MemoryGraph", SOURCE_TEXT)
        self.assertIn('format="{Available: .1f}{mm} free"', SOURCE_TEXT)

    def test_exactly_one_legacy_systray_and_notifications_every_role(self):
        self.assertEqual(SOURCE_TEXT.count("widget.Systray"), 1)
        self.assertIn('cmd=["python3", script, "--status"]', SOURCE_TEXT)
        self.assertIn('lazy.spawn(f"python3 {shlex.quote(script)} --menu")', SOURCE_TEXT)
        self.assertIn('lazy.spawn("dunstctl set-paused toggle")', SOURCE_TEXT)
        self.assertIn("items.append(_notification_widget(config_globals, role))", SOURCE_TEXT)

    def test_volume_uses_outrun_red(self):
        self.assertIn('widget.Volume(foreground=palette["red"]', SOURCE_TEXT)

    def test_org_poll_uses_async_genpollcommand(self):
        self.assertIn("widget.GenPollCommand", SOURCE_TEXT)
        self.assertIn('name="org_clocked_task"', SOURCE_TEXT)

    def test_left_org_screen_has_gpt_todo_sync_button(self):
        left = SOURCE_TEXT.index('elif role == "left":')
        right = SOURCE_TEXT.index('elif role == "right":')
        left_text = SOURCE_TEXT[left:right]
        self.assertIn('name="gpt_todos_sync_button"', left_text)
        self.assertIn("lazy.function(_sync_gpt_todos)", left_text)
        self.assertIn('text=" 󰑓 SYNC "', left_text)

    def test_gpt_todo_sync_is_off_event_loop_and_notifies(self):
        self.assertIn('threading.Thread(target=worker, name="qtile-gpt-todos-sync", daemon=True).start()', SOURCE_TEXT)
        self.assertIn('["bash", str(GPT_TODOS_SYNC)]', SOURCE_TEXT)
        self.assertIn('"Synchronizing all agenda files…"', SOURCE_TEXT)
        self.assertIn('"GPT TODO sync complete"', SOURCE_TEXT)
        self.assertIn('"GPT TODO sync failed"', SOURCE_TEXT)

    def test_pomodoro_lives_on_left_with_n1_center_fallback(self):
        center = SOURCE_TEXT.index('if role == "center":')
        left = SOURCE_TEXT.index('elif role == "left":')
        right = SOURCE_TEXT.index('elif role == "right":')
        center_text = SOURCE_TEXT[center:left]
        left_text = SOURCE_TEXT[left:right]
        self.assertIn("if show_date:", center_text)
        self.assertEqual(center_text.count("widget.Pomodoro"), 1)
        self.assertEqual(left_text.count("widget.Pomodoro"), 1)
        self.assertEqual(SOURCE_TEXT.count("widget.Pomodoro"), 2)

    def test_clock_icons_single_date_and_click_actions(self):
        self.assertEqual(SOURCE_TEXT.count('"󰃭 %Y-%m-%d   %H:%M"'), 1)
        self.assertEqual(SOURCE_TEXT.count('" %H:%M"'), 1)
        self.assertIn('dropdown_toggle("org-agenda-day")', SOURCE_TEXT)
        self.assertIn("lazy.function(_show_month_calendar)", SOURCE_TEXT)
        self.assertIn("org-agenda-list nil (current-time) 1", SOURCE_TEXT)
        expected = MODULE.month_calendar_text(datetime(2026, 8, 23, 12, 0))
        self.assertIn("August 2026", expected)
        self.assertIn("23", expected)

    def test_generated_layout_puts_date_on_left_or_center_for_n1(self):
        self.assertIn('date_role = "left" if any(base_role(role) == "left" for role in roles) else "center"', SOURCE_TEXT)
        self.assertIn('show_date=base_role(role) == date_role', SOURCE_TEXT)
        self.assertIn('show_date = role == "center"', SOURCE_TEXT)

    def test_agenda_and_todo_dropdowns_are_right_aligned(self):
        self.assertIn('"org-agenda-day"', SOURCE_TEXT)
        self.assertIn('x=0.38,\n                    y=0.02,', SOURCE_TEXT)
        self.assertIn('x=0.42,\n                    y=0.02,', SOURCE_TEXT)
        self.assertIn('x=0.19,\n                    y=0.04,', SOURCE_TEXT)
        self.assertIn('qtile-workflow.el', SOURCE_TEXT)
        self.assertIn("qtile-workflow-read-right", SOURCE_TEXT)


if __name__ == "__main__":
    unittest.main()
