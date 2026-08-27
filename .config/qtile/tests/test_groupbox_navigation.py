#!/usr/bin/env python3
"""Widget-level regression tests for OwnedGroupBox navigation and geometry.

The libqtile stub's GroupBox base carries the verbatim Qtile 0.33.0
next_group/prev_group implementation, which spins forever when
self.groups is empty. If the OwnedGroupBox overrides are removed, these
tests hang and are failed by the alarm instead of blocking CI forever.
"""

from __future__ import annotations

import contextlib
import importlib.util
import itertools
import signal
import sys
import types
import unittest
from dataclasses import dataclass, field
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[1] / "qtile_control.py"
SPEC = importlib.util.spec_from_file_location("qtile_control_nav", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


@dataclass
class Group:
    name: str
    label: str
    windows: list = field(default_factory=list)
    screen: object | None = None


@contextlib.contextmanager
def timeout_guard(seconds: float = 2.0):
    """Fail loudly instead of hanging when navigation regresses to a spin."""

    def _fail(signum, frame):
        raise AssertionError("group navigation did not terminate (infinite loop)")

    previous = signal.signal(signal.SIGALRM, _fail)
    signal.setitimer(signal.ITIMER_REAL, seconds)
    try:
        yield
    finally:
        signal.setitimer(signal.ITIMER_REAL, 0)
        signal.signal(signal.SIGALRM, previous)


class FakeScreen:
    def __init__(self):
        self.set_group_calls = []

    def set_group(self, group, warp=None):
        self.set_group_calls.append(group)


class FakeBar:
    def __init__(self, screen):
        self.screen = screen


class FakeQtile:
    def __init__(self, groups, current_group, current_screen):
        self.groups = groups
        self.current_group = current_group
        self.current_screen = current_screen


class _HookProxy:
    def __getattr__(self, name):
        def _noop(*args, **kwargs):
            return None

        return _noop


def _install_libqtile_stub():
    libqtile = types.ModuleType("libqtile")
    widget_mod = types.ModuleType("libqtile.widget")
    hook_mod = types.ModuleType("libqtile.hook")
    hook_mod.subscribe = _HookProxy()
    hook_mod.unsubscribe = _HookProxy()

    class _GroupBox:
        # Minimal constructor mirroring Qtile's config-kwargs pattern.
        def __init__(self, **config):
            self.__dict__.update(config)
            self.visible_groups = config.get("visible_groups")

        # Qtile 0.33.0 libqtile/widget/groupbox.py, verbatim.
        def next_group(self):
            group = None
            current_group = self.qtile.current_group
            i = itertools.cycle(self.qtile.groups)
            while next(i) != current_group:
                pass
            while group is None or group not in self.groups:
                group = next(i)
            self.go_to_group(group)

        def prev_group(self):
            group = None
            current_group = self.qtile.current_group
            i = itertools.cycle(reversed(self.qtile.groups))
            while next(i) != current_group:
                pass
            while group is None or group not in self.groups:
                group = next(i)
            self.go_to_group(group)

    widget_mod.GroupBox = _GroupBox
    libqtile.widget = widget_mod
    libqtile.hook = hook_mod
    sys.modules["libqtile"] = libqtile
    sys.modules["libqtile.widget"] = widget_mod
    sys.modules["libqtile.hook"] = hook_mod


def _remove_libqtile_stub():
    for name in ("libqtile", "libqtile.widget", "libqtile.hook"):
        sys.modules.pop(name, None)


class OwnedGroupBoxNavigationTests(unittest.TestCase):
    def setUp(self):
        MODULE._group_owner_roles.clear()
        _install_libqtile_stub()
        self.addCleanup(_remove_libqtile_stub)

    def _build_box(self, group_names=("1", "2", "3")):
        colors = [[str(index)] * 2 for index in range(10)]
        box = MODULE._owned_group_box(
            {"colors": colors, "group_names": list(group_names)}
        )
        screen = FakeScreen()
        box.bar = FakeBar(screen)
        navigated = []

        def _navigate(group):
            navigated.append(group)
            # Real Qtile advances the current group via Screen.set_group.
            box.qtile.current_group = group
            box.bar.screen.group = group

        box.go_to_group = _navigate
        return box, navigated

    def test_navigation_terminates_when_no_group_has_windows(self):
        box, navigated = self._build_box()
        empty = [Group("1", "one", []), Group("2", "two", [])]
        box.qtile = FakeQtile(empty, empty[0], box.bar.screen)
        with timeout_guard():
            box.next_group()
            box.prev_group()
        self.assertEqual(navigated, [])

    def test_group_icons_use_the_font_that_owns_their_metrics(self):
        box, _ = self._build_box()
        self.assertEqual(box.font, "Symbols Nerd Font")

    def test_next_group_skips_windowless_groups(self):
        box, navigated = self._build_box()
        groups = [
            Group("1", "one", [object()]),
            Group("2", "two", []),
            Group("3", "three", [object()]),
        ]
        box.qtile = FakeQtile(groups, groups[0], box.bar.screen)
        with timeout_guard():
            box.next_group()
        self.assertIs(navigated[-1], groups[2])

    def test_next_group_wraps_around(self):
        box, navigated = self._build_box()
        groups = [
            Group("1", "one", [object()]),
            Group("2", "two", [object()]),
            Group("3", "three", [object()]),
        ]
        box.qtile = FakeQtile(groups, groups[2], box.bar.screen)
        with timeout_guard():
            box.next_group()
        self.assertIs(navigated[-1], groups[0])

    def test_prev_group_wraps_around(self):
        box, navigated = self._build_box()
        groups = [
            Group("1", "one", [object()]),
            Group("2", "two", [object()]),
            Group("3", "three", [object()]),
        ]
        box.qtile = FakeQtile(groups, groups[0], box.bar.screen)
        with timeout_guard():
            box.prev_group()
        self.assertIs(navigated[-1], groups[2])

    def test_navigation_from_empty_current_group_lands_on_visible_group(self):
        box, navigated = self._build_box()
        groups = [
            Group("1", "one", [object()]),
            Group("2", "two", []),
            Group("3", "three", [object()]),
        ]
        box.qtile = FakeQtile(groups, groups[1], box.bar.screen)
        with timeout_guard():
            box.next_group()
        self.assertIs(navigated[-1], groups[0])
        with timeout_guard():
            box.prev_group()
        self.assertIs(navigated[-1], groups[2])

    def test_visible_groups_attribute_restricts_navigation(self):
        box, navigated = self._build_box(group_names=("1", "3"))
        groups = [
            Group("1", "one", [object()]),
            Group("2", "two", [object()]),
            Group("3", "three", [object()]),
        ]
        box.qtile = FakeQtile(groups, groups[0], box.bar.screen)
        self.assertEqual([group.name for group in box.groups], ["1", "3"])
        with timeout_guard():
            box.next_group()
        self.assertIs(navigated[-1], groups[2])

    def test_single_visible_group_navigates_to_itself(self):
        box, navigated = self._build_box(group_names=("2",))
        groups = [
            Group("1", "one", []),
            Group("2", "two", [object()]),
        ]
        box.qtile = FakeQtile(groups, groups[1], box.bar.screen)
        with timeout_guard():
            box.next_group()
        self.assertIs(navigated[-1], groups[1])

    def test_navigation_with_all_windows_on_all_groups_n_screens(self):
        # Simulate N=4 groups spread over roles; navigation must visit
        # every window-owning group and terminate each time.
        box, navigated = self._build_box(group_names=("1", "2", "3", "4"))
        groups = [Group(str(i), str(i), [object()]) for i in range(1, 5)]
        box.qtile = FakeQtile(groups, groups[0], box.bar.screen)
        visited = [groups[0]]
        with timeout_guard():
            for _ in range(3):
                box.next_group()
                visited.append(navigated[-1])
        self.assertEqual(
            [group.name for group in visited], ["1", "2", "3", "4"]
        )

    def test_next_visible_group_pure_function_edges(self):
        self.assertIsNone(MODULE.next_visible_group([], object()))
        sentinel = object()
        self.assertIs(MODULE.next_visible_group([sentinel], sentinel), sentinel)
        self.assertIs(MODULE.next_visible_group([sentinel], object()), sentinel)

    def test_group_indicator_insets_padding_without_moving_center(self):
        offset, width = MODULE.group_indicator_geometry(100, 32, 6, 2)
        self.assertEqual((offset, width), (108, 16))
        self.assertEqual(100 + 32 / 2, offset + width / 2)

    def test_group_indicator_does_not_collapse_tiny_boxes(self):
        self.assertEqual(MODULE.group_indicator_geometry(5, 8, 6, 2), (5, 8))


if __name__ == "__main__":
    unittest.main()
