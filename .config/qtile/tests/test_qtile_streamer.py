#!/usr/bin/env python3
"""Regression tests for streamer mode: allow-list, enforcement, and recording."""

from __future__ import annotations

import sys
import unittest
from dataclasses import dataclass, field
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / ".config" / "qtile"))

from qtile_ai_windows import STREAM_ALLOWLIST, is_stream_allowed  # noqa: E402
import qtile_streamer  # noqa: E402


@dataclass
class Rect:
    x: int
    width: int


class FakeScreen:
    def __init__(self, x, width=1920):
        self.rect = Rect(x, width)


@dataclass
class FakeGroup:
    name: str
    screen: FakeScreen | None = None
    windows: list = field(default_factory=list)


class FakeXWindow:
    def __init__(self, wm_classes):
        self._classes = wm_classes

    def get_wm_class(self):
        return self._classes


@dataclass
class FakeWindow:
    group: FakeGroup
    name: str = ""
    classes: tuple = ()

    def __post_init__(self):
        self.window = FakeXWindow(self.classes)
        self.moved_to = None

    def togroup(self, group_name, switch_group=False):
        self.moved_to = (group_name, switch_group)


class FakeQtile:
    def __init__(self):
        self.screens = [
            FakeScreen(0),
            FakeScreen(1920),
            FakeScreen(3840),
        ]
        self.groups = [
            FakeGroup("1", self.screens[0]),
            FakeGroup("2", self.screens[1]),
            FakeGroup("6", self.screens[2]),
        ]
        self.groups_map = {group.name: group for group in self.groups}
        self.current_window = None

    def add_group(self, name, layout=None, label=None, persist=False):
        if name in self.groups_map:
            return False
        group = FakeGroup(name)
        self.groups.append(group)
        self.groups_map[name] = group
        return True


class StreamAllowlistTests(unittest.TestCase):
    def test_allowlisted_windows_match(self):
        self.assertTrue(is_stream_allowed(("Nyxt", "nyxt"), "Nyxt Web Browser"))
        self.assertTrue(is_stream_allowed(("StreamEmacs",), "StreamEmacs"))
        self.assertTrue(is_stream_allowed(("emacs", "Emacs"), "StreamEmacs"))
        self.assertTrue(is_stream_allowed(("terminator", "Terminator"), "OpenCode"))
        self.assertTrue(is_stream_allowed(("terminator",), "opencode session"))

    def test_module_constant_lists_the_stream_allow_entries(self):
        self.assertIn("nyxt", STREAM_ALLOWLIST["classes"])
        self.assertIn("streamemacs", STREAM_ALLOWLIST["classes"])
        self.assertIn("emacs", STREAM_ALLOWLIST["title_classes"])
        self.assertIn("terminator", STREAM_ALLOWLIST["title_classes"])
        patterns = [pattern.pattern for pattern in STREAM_ALLOWLIST["title_patterns"]]
        self.assertTrue(any("opencode" in pattern for pattern in patterns))
        self.assertTrue(any("streamemacs" in pattern for pattern in patterns))

    def test_everyday_private_windows_are_rejected(self):
        for wm_classes, title in (
            (("brave-browser", "Brave-browser"), "GitHub - Brave"),
            (("terminator", "Terminator"), "unseen@flake:~/Documents"),
            (("keepassxc", "KeePassXC"), "KeePassXC - Passwords"),
            (("discord",), "Discord"),
            (("thunderbird",), "Mail"),
            ((), "Totally private doc"),
        ):
            with self.subTest(wm_classes=wm_classes, title=title):
                self.assertFalse(is_stream_allowed(wm_classes, title))


class StreamerEnforcementTests(unittest.TestCase):
    def setUp(self):
        self._patches = [
            mock.patch.object(qtile_streamer, "_notify", lambda *a, **k: None),
            mock.patch.object(qtile_streamer, "_telemetry_event", lambda *a, **k: None),
            mock.patch.object(qtile_streamer, "_telemetry_auto_route", lambda *a, **k: None),
        ]
        for patcher in self._patches:
            patcher.start()
        qtile_streamer._state.update(enabled=False, recording=False)
        qtile_streamer._buttons.clear()

    def tearDown(self):
        for patcher in self._patches:
            patcher.stop()
        qtile_streamer._state.update(enabled=False, recording=False)
        qtile_streamer._buttons.clear()

    def test_center_role_resolution_uses_screen_roles(self):
        qtile = FakeQtile()
        self.assertEqual(qtile_streamer.center_screen_index(qtile), 1)
        empty = FakeQtile()
        empty.screens = []
        self.assertIsNone(qtile_streamer.center_screen_index(empty))

    def test_non_allowed_window_is_moved_off_center_when_enabled(self):
        qtile = FakeQtile()
        brave = FakeWindow(qtile.groups_map["2"], "GitHub - Brave", ("brave-browser",))
        qtile.groups_map["2"].windows.append(brave)

        self.assertFalse(qtile_streamer.enforce_streamer(qtile, brave))
        self.assertIsNone(brave.moved_to)

        qtile_streamer.set_streamer_mode(qtile, True)
        self.assertTrue(qtile_streamer.is_enabled())
        self.assertTrue(qtile_streamer.enforce_streamer(qtile, brave))
        self.assertEqual(brave.moved_to, ("hold", False))
        self.assertIn("hold", qtile.groups_map)

    def test_allowed_windows_and_off_center_windows_stay(self):
        qtile = FakeQtile()
        nyxt = FakeWindow(qtile.groups_map["2"], "Nyxt", ("Nyxt",))
        opencode = FakeWindow(qtile.groups_map["2"], "OpenCode", ("terminator",))
        keepass = FakeWindow(qtile.groups_map["6"], "KeePassXC", ("keepassxc",))
        qtile.groups_map["2"].windows.extend([nyxt, opencode])
        qtile.groups_map["6"].windows.append(keepass)

        qtile_streamer.set_streamer_mode(qtile, True)
        self.assertFalse(qtile_streamer.enforce_streamer(qtile, nyxt))
        self.assertFalse(qtile_streamer.enforce_streamer(qtile, opencode))
        self.assertFalse(qtile_streamer.enforce_streamer(qtile, keepass))
        self.assertIsNone(nyxt.moved_to)
        self.assertIsNone(opencode.moved_to)
        self.assertIsNone(keepass.moved_to)

    def test_enabling_sweeps_existing_violations_off_center(self):
        qtile = FakeQtile()
        brave = FakeWindow(qtile.groups_map["2"], "Brave", ("brave-browser",))
        keepass = FakeWindow(qtile.groups_map["6"], "KeePassXC", ("keepassxc",))
        nyxt = FakeWindow(qtile.groups_map["2"], "Nyxt", ("Nyxt",))
        qtile.groups_map["2"].windows.extend([brave, nyxt])
        qtile.groups_map["6"].windows.append(keepass)

        moved = qtile_streamer.set_streamer_mode(qtile, True)
        self.assertTrue(moved)
        self.assertEqual(brave.moved_to, ("hold", False))
        self.assertIsNone(nyxt.moved_to)
        self.assertIsNone(keepass.moved_to)

    def test_disabling_stops_enforcement_and_recording(self):
        qtile = FakeQtile()
        qtile_streamer.set_streamer_mode(qtile, True)
        with mock.patch.object(
            qtile_streamer, "_spawn_capture", return_value=True
        ) as spawn:
            qtile_streamer._state["recording"] = True
            qtile_streamer.set_streamer_mode(qtile, False)
            spawn.assert_called_once_with(qtile_streamer.STOP_SUBCOMMAND)
        self.assertFalse(qtile_streamer.is_enabled())
        self.assertFalse(qtile_streamer.is_recording())

        brave = FakeWindow(qtile.groups_map["2"], "Brave", ("brave-browser",))
        self.assertFalse(qtile_streamer.enforce_streamer(qtile, brave))
        self.assertIsNone(brave.moved_to)

    def test_toggle_flips_the_session_only_state(self):
        qtile = FakeQtile()
        qtile_streamer.toggle_streamer_mode(qtile)
        self.assertTrue(qtile_streamer.is_enabled())
        qtile_streamer.toggle_streamer_mode(qtile)
        self.assertFalse(qtile_streamer.is_enabled())


class StreamerRecordingTests(unittest.TestCase):
    def setUp(self):
        self._patches = [
            mock.patch.object(qtile_streamer, "_notify", lambda *a, **k: None),
            mock.patch.object(qtile_streamer, "_telemetry_event", lambda *a, **k: None),
        ]
        for patcher in self._patches:
            patcher.start()
        qtile_streamer._state.update(enabled=False, recording=False)
        qtile_streamer._buttons.clear()

    def tearDown(self):
        for patcher in self._patches:
            patcher.stop()
        qtile_streamer._state.update(enabled=False, recording=False)
        qtile_streamer._buttons.clear()

    def test_toggle_with_record_spawns_the_screen_capture_cli_without_blocking(self):
        qtile = FakeQtile()
        with mock.patch.object(qtile_streamer.subprocess, "Popen") as popen:
            popen.return_value = object()
            qtile_streamer.toggle_streamer_mode(qtile, record=True)
            self.assertEqual(
                popen.call_args_list[-1].args[0],
                [
                    qtile_streamer.STREAM_CAPTURE_SCRIPT,
                    qtile_streamer.RECORD_SUBCOMMAND,
                ],
            )
            self.assertTrue(popen.call_args_list[-1].kwargs["start_new_session"])
            self.assertTrue(qtile_streamer.is_recording())

            qtile_streamer.toggle_streamer_mode(qtile, record=True)
            self.assertEqual(
                popen.call_args_list[-1].args[0],
                [
                    qtile_streamer.STREAM_CAPTURE_SCRIPT,
                    qtile_streamer.STOP_SUBCOMMAND,
                ],
            )
            self.assertFalse(qtile_streamer.is_recording())

    def test_manual_recording_is_left_alone_by_streamer_toggle(self):
        qtile = FakeQtile()
        with mock.patch.object(qtile_streamer, "_spawn_capture", return_value=True) as spawn:
            qtile_streamer.toggle_streamer_mode(qtile, record=False)
            spawn.assert_not_called()
            qtile_streamer.toggle_streamer_mode(qtile, record=False)
            spawn.assert_not_called()

    def test_failed_spawn_keeps_state_truthful(self):
        with mock.patch.object(qtile_streamer.subprocess, "Popen", side_effect=OSError("nope")):
            self.assertFalse(qtile_streamer.start_recording())
            self.assertFalse(qtile_streamer.is_recording())


class StreamerButtonTests(unittest.TestCase):
    def setUp(self):
        import types

        libqtile = types.ModuleType("libqtile")
        widget_mod = types.ModuleType("libqtile.widget")
        lazy_mod = types.ModuleType("libqtile.lazy")

        class TextBox:
            def __init__(self, **config):
                self.__dict__.update(config)

            def draw(self):
                self.drawn = True

        lazy_obj = types.SimpleNamespace(function=lambda function, *args, **kwargs: function)
        lazy_mod.lazy = lazy_obj
        widget_mod.TextBox = TextBox
        libqtile.widget = widget_mod
        sys.modules["libqtile"] = libqtile
        sys.modules["libqtile.widget"] = widget_mod
        sys.modules["libqtile.lazy"] = lazy_mod
        self.addCleanup(self._remove_stub)
        qtile_streamer._state.update(enabled=False, recording=False)
        qtile_streamer._buttons.clear()

    def _remove_stub(self):
        for name in ("libqtile", "libqtile.widget", "libqtile.lazy"):
            sys.modules.pop(name, None)
        qtile_streamer._state.update(enabled=False, recording=False)
        qtile_streamer._buttons.clear()

    def test_button_mirrors_the_auto_button_pattern(self):
        config = {
            "colors": [[str(index)] * 2 for index in range(10)],
        }
        button = qtile_streamer.streamer_button(config)
        self.assertEqual(button.text, " STREAM ")
        self.assertIn("Button1", button.mouse_callbacks)
        self.assertEqual(button.foreground, "#000000")

        qtile_streamer.update_streamer_buttons()
        self.assertEqual(button.background, qtile_streamer._button_background)

        qtile_streamer._state["enabled"] = True
        qtile_streamer.update_streamer_buttons()
        self.assertEqual(button.background, qtile_streamer._button_armed)
        self.assertTrue(button.drawn)

    def test_disabled_state_is_the_default_on_module_state_reset(self):
        self.assertFalse(qtile_streamer.is_enabled())


def _load_qtile_control():
    import importlib.util

    spec = importlib.util.spec_from_file_location(
        "qtile_control_under_test", ROOT / ".config" / "qtile" / "qtile_control.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _libqtile_available():
    try:
        import libqtile  # noqa: F401
    except ImportError:
        return False
    return True


class StreamerBarWiringTests(unittest.TestCase):
    def setUp(self):
        self._saved_modules = {
            name: module
            for name, module in sys.modules.items()
            if name == "libqtile" or name.startswith("libqtile.")
        }
        for name in list(self._saved_modules):
            sys.modules.pop(name, None)
        # The bar wiring under test only needs `from libqtile import
        # widget` to resolve, so CI pythons without libqtile get a mock;
        # leftover stub modules from other suites are always replaced.
        sys.modules["libqtile"] = mock.MagicMock(name="libqtile-stub")

    def tearDown(self):
        for name in list(sys.modules):
            if name == "libqtile" or name.startswith("libqtile."):
                sys.modules.pop(name, None)
        sys.modules.update(self._saved_modules)

    def test_base_widgets_invokes_streamer_button_with_config_globals(self):
        control = _load_qtile_control()

        seen = {}

        def fake_streamer_button(config_globals):
            seen["globals"] = config_globals
            return "STREAM_MARKER"

        config_globals = {
            "colors": [f"#{index:06x}" for index in range(10)],
            "group_names": ["1", "2"],
            "auto_group_button": None,
            "streamer_button": fake_streamer_button,
        }

        items = control._base_widgets(config_globals)

        self.assertIn("STREAM_MARKER", items)
        self.assertIs(seen["globals"], config_globals)


if __name__ == "__main__":
    unittest.main()
