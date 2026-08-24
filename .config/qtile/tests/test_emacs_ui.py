#!/usr/bin/env python3
"""Behavioral tests for widget-relative Emacs popup geometry and launch."""

from __future__ import annotations

import importlib.util
import shutil
import subprocess
import sys
import unittest
from dataclasses import dataclass
from pathlib import Path
from unittest import mock

SOURCE = Path(__file__).resolve().parents[1] / "emacs_ui.py"
SPEC = importlib.util.spec_from_file_location("emacs_ui", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


@dataclass
class Rect:
    x: int
    y: int
    width: int
    height: int


class Bar:
    position = "top"
    height = 26

    def __init__(self, screen):
        self.screen = screen
        self.widgets = []


class Screen:
    def __init__(self, x=0, y=0, width=1920, height=1080):
        self.rect = Rect(x, y, width, height)
        self.top = None


class Widget:
    def __init__(self, bar, name="trigger", x=100, width=80):
        self.bar = bar
        self.name = name
        # Qtile bar widgets expose offset/length; x/width are the legacy path.
        self.offset = x
        self.length = width
        self.height = bar.height


class Qtile:
    def __init__(self, screens):
        self.screens = screens


def qtile_with_widget(*, screen=None, x=100, width=80, name="trigger"):
    screen = screen or Screen()
    bar = Bar(screen)
    screen.top = bar
    candidate = Widget(bar, name=name, x=x, width=width)
    bar.widgets.append(candidate)
    return Qtile([screen])


class EmacsUiTests(unittest.TestCase):
    def test_widget_lookup_and_absolute_geometry_include_monitor_origin(self):
        qtile = qtile_with_widget(screen=Screen(1920, 40), x=120)
        self.assertEqual(
            MODULE.widget_geometry(qtile, "trigger"),
            (qtile.screens[0], 2040, 40, 80, 26),
        )

    def test_alignments_attach_to_bar_bottom(self):
        qtile = qtile_with_widget(x=100, width=100)
        self.assertEqual(MODULE.popup_geometry(qtile, "trigger", width=400, height=500, align="left").left, 100)
        self.assertEqual(MODULE.popup_geometry(qtile, "trigger", width=400, height=500, align="center").left, 0)
        self.assertEqual(MODULE.popup_geometry(qtile, "trigger", width=400, height=500, align="right").left, 0)
        self.assertEqual(MODULE.popup_geometry(qtile, "trigger", width=400, height=500, align="left").top, 26)

    def test_left_and_right_edges_are_clamped_to_owning_screen(self):
        qtile = qtile_with_widget(screen=Screen(1000, 0, 800, 600), x=0, width=20)
        left = MODULE.popup_geometry(qtile, "trigger", width=400, height=300, align="right")
        self.assertEqual(left.left, 1000)
        qtile = qtile_with_widget(screen=Screen(1000, 0, 800, 600), x=780, width=20)
        right = MODULE.popup_geometry(qtile, "trigger", width=400, height=300, align="left")
        self.assertEqual(right.left, 1400)

    def test_n1_to_n4_topology_uses_trigger_screen(self):
        screens = [Screen(index * 800, 0, 800, 600) for index in range(4)]
        for index, screen in enumerate(screens):
            bar = Bar(screen)
            screen.top = bar
            bar.widgets.append(Widget(bar, name=f"widget-{index}", x=10))
        for count in range(1, 5):
            qtile = Qtile(screens[:count])
            geometry = MODULE.popup_geometry(qtile, f"widget-{count - 1}", width=300, height=200)
            self.assertEqual(geometry.screen_x, (count - 1) * 800)

    def test_changed_widget_width_recomputes_right_alignment(self):
        qtile = qtile_with_widget(x=400, width=80)
        first = MODULE.popup_geometry(qtile, "trigger", width=200, height=200, align="right")
        qtile.screens[0].top.widgets[0].length = 140
        second = MODULE.popup_geometry(qtile, "trigger", width=200, height=200, align="right")
        self.assertEqual(second.left - first.left, 60)

    def test_invalid_alignment_is_rejected(self):
        with self.assertRaises(ValueError):
            MODULE.popup_geometry(qtile_with_widget(), "trigger", width=10, height=10, align="diagonal")

    def test_command_uses_structured_arguments_without_shell_coordinates(self):
        geometry = MODULE.PopupGeometry(10, 26, 400, 300, 0, 0, 800, 600)
        command = MODULE.build_emacsclient_command(
            popup_id="notifications",
            function="qtile-notifications-open",
            geometry=geometry,
            args={"backend": "emacs"},
            minibuffer=True,
        )
        self.assertEqual(command[:5], ["emacsclient", "-s", "qtile", "-a", "false"])
        self.assertIn("json-read-from-string", command[-1])
        self.assertNotIn("shell=True", command[-1])
        self.assertIn("notifications", command[-1])
        self.assertIn("minibuffer", command[-1])
        self.assertIn("display", command[-1])
        self.assertIn("load-path", command[-1])

    def test_dropdown_command_uses_a_private_minibuffer_by_default(self):
        geometry = MODULE.PopupGeometry(10, 26, 400, 300, 0, 0, 800, 600)
        command = MODULE.build_emacsclient_command(
            popup_id="notifications",
            function="qtile-notifications-open",
            geometry=geometry,
        )
        self.assertIn("minibuffer", command[-1])
        self.assertIn("true", command[-1])

    def test_widget_offset_drives_alignment_not_a_defaulted_x(self):
        qtile = qtile_with_widget(x=777)
        screen, x, y, width, height = MODULE.widget_geometry(qtile, "trigger")
        self.assertEqual(x, 777)

    def test_legacy_widgets_without_offset_still_align(self):
        qtile = qtile_with_widget(x=333)
        widget = qtile.screens[0].top.widgets[0]
        del widget.offset
        del widget.length
        widget.x = 333
        widget.width = 90
        self.assertEqual(MODULE.widget_geometry(qtile, "trigger")[1], 333)
        self.assertEqual(MODULE.widget_geometry(qtile, "trigger")[3], 90)

    def test_missing_server_starts_headless_daemon_and_retries(self):
        missing = subprocess.CompletedProcess(
            ["emacsclient"], 1, "", "emacsclient: can't find socket; have you started the server?"
        )
        success = subprocess.CompletedProcess(["emacsclient"], 0, "nil", "")
        with mock.patch.object(MODULE, "_try_client", side_effect=[missing, success]) as client, \
             mock.patch.object(MODULE, "_start_named_daemon", return_value=True) as daemon, \
             mock.patch.object(MODULE, "_notify_user") as notify, \
             mock.patch.object(MODULE, "_report_error"):
            MODULE._run_client(object(), ["emacsclient", "-s", "qtile", "--eval", "(+ 1 1)"])
        daemon.assert_called_once_with()
        self.assertEqual(client.call_count, 2)
        notify.assert_not_called()

    def test_missing_server_never_spawns_emacs_and_notifies_user(self):
        missing = subprocess.CompletedProcess(
            ["emacsclient"], 1, "", "emacsclient: can't find socket; have you started the server?"
        )
        with mock.patch.object(MODULE, "_try_client", return_value=missing), \
             mock.patch.object(MODULE, "_start_named_daemon", return_value=False), \
             mock.patch.object(MODULE, "_notify_user") as notify, \
             mock.patch.object(MODULE, "_report_error"):
            MODULE._run_client(object(), ["emacsclient", "-s", "qtile", "--eval", "(+ 1 1)"])
        notify.assert_called_once()
        self.assertIn("server", notify.call_args.args[1].lower())

    def test_daemon_start_uses_named_headless_daemon(self):
        emacs = shutil.which("emacs")
        if emacs is None:
            self.skipTest("emacs not on PATH")
        completed = subprocess.CompletedProcess([], 0, "Starting Emacs daemon.", "")
        with mock.patch.object(MODULE.shutil, "which", return_value="/usr/bin/emacs"), \
             mock.patch.object(MODULE.subprocess, "run", return_value=completed) as run:
            self.assertTrue(MODULE._start_named_daemon())
        self.assertEqual(run.call_args.args[0], ["/usr/bin/emacs", "--daemon=qtile"])
        self.assertNotIn("-c", run.call_args.args[0])

    def test_launch_is_scheduled_on_worker_without_waiting_in_qtile(self):
        qtile = qtile_with_widget()
        fake_thread = mock.Mock()
        with mock.patch.object(MODULE.threading, "Thread", return_value=fake_thread) as thread:
            result = MODULE.toggle_emacs_dropdown(
                qtile,
                widget_name="trigger",
                popup_id="a0",
                function="qtile-agent-zero-open",
                width=400,
                height=300,
            )
        self.assertTrue(result.started)
        thread.assert_called_once()
        fake_thread.start.assert_called_once_with()

    def test_missing_widget_is_reported_without_launching(self):
        result = MODULE.toggle_emacs_dropdown(
            Qtile([]),
            widget_name="missing",
            popup_id="a0",
            function="qtile-agent-zero-open",
            width=400,
            height=300,
        )
        self.assertFalse(result.started)
        self.assertIn("missing", result.reason)


if __name__ == "__main__":
    unittest.main()
