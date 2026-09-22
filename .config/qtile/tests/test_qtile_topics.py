#!/usr/bin/env python3
"""Regression tests for the generated topic group pool and its runtime."""

from __future__ import annotations

import importlib.util
import sys
import types
import unittest
from dataclasses import dataclass, field
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / ".config" / "qtile"))

from qtile_topics import (  # noqa: E402
    TOPIC_GROUP_PREFIX,
    build_topic_groups,
    create_topic,
    expose_topic_commands,
    focus_topic,
    is_topic_group,
    next_topic,
    normalize_topic_name,
    previous_topic,
    registered_topic_names,
    remove_topic,
    send_current_to_topic,
    topic_group_name,
    topic_labels,
)

from qtile_control import _owned_group_box  # noqa: E402


@dataclass
class FakeGroup:
    name: str
    label: str = ""
    windows: list = field(default_factory=list)
    screen: object | None = None
    qtile_ref: object | None = None
    toscreen_calls: int = 0

    def toscreen(self):
        self.toscreen_calls += 1
        if self.qtile_ref is not None:
            self.qtile_ref.current_group = self


@dataclass
class FakeWindow:
    group: FakeGroup
    name: str = "window"

    def togroup(self, group_name, switch_group=False):
        self.group.windows.remove(self)
        self.group = FakeGroup(group_name, group_name, [self])
        self.moved_to = (group_name, switch_group)


class FakeQtile:
    def __init__(self, groups):
        self.groups = list(groups)
        self.groups_map = {group.name: group for group in groups}
        self.current_group = groups[0] if groups else None
        self.current_window = None
        self.added = []
        self.deleted = []
        for group in self.groups:
            group.qtile_ref = self

    def add_group(self, name, layout=None, label=None, persist=False):
        if name in self.groups_map:
            return False
        group = FakeGroup(name, label or name, qtile_ref=self)
        self.groups.append(group)
        self.groups_map[name] = group
        self.added.append({"name": name, "layout": layout, "label": label})
        return True

    def delete_group(self, name):
        group = self.groups_map.pop(name)
        self.groups.remove(group)
        self.deleted.append(name)

    def call_soon_threadsafe(self, function, *arguments):
        function(*arguments)


def _install_libqtile_stub():
    libqtile = types.ModuleType("libqtile")
    config_mod = types.ModuleType("libqtile.config")

    class Group:
        def __init__(self, name, layout=None, label=None, **kwargs):
            self.name = name
            self.layout = layout
            self.label = label

    config_mod.Group = Group
    libqtile.config = config_mod
    sys.modules["libqtile"] = libqtile
    sys.modules["libqtile.config"] = config_mod


def _remove_libqtile_stub():
    sys.modules.pop("libqtile", None)
    sys.modules.pop("libqtile.config", None)


def _control_module():
    source = ROOT / ".config" / "qtile" / "qtile_control.py"
    spec = importlib.util.spec_from_file_location("qtile_control_topics", source)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


class TopicNameTests(unittest.TestCase):
    def test_topic_group_names_are_prefixed_and_idempotent(self):
        self.assertEqual(topic_group_name("zara"), "t:zara")
        self.assertEqual(topic_group_name("t:zara"), "t:zara")
        self.assertTrue(is_topic_group("t:zara"))
        self.assertFalse(is_topic_group("3"))

    def test_normalize_topic_name_rejects_unsafe_characters(self):
        self.assertEqual(normalize_topic_name("My Topic!"), "my-topic")
        self.assertEqual(normalize_topic_name("--prolog_rlm.9--"), "prolog_rlm.9")
        self.assertEqual(normalize_topic_name("///"), "")


class TopicGenerationTests(unittest.TestCase):
    def setUp(self):
        _install_libqtile_stub()
        self.addCleanup(_remove_libqtile_stub)
        self._reset_registries()

    @staticmethod
    def _reset_registries():
        import qtile_topics

        qtile_topics._default_topics.clear()
        qtile_topics._runtime_topics.clear()
        qtile_topics._topic_layouts.clear()

    def tearDown(self):
        self._reset_registries()

    def test_generation_creates_prefixed_groups_after_static_ones(self):
        definitions = [
            ("zara", "Zara", "monadtall"),
            ("media", "Media", "max"),
            ("bare",),
        ]
        groups = build_topic_groups(definitions)
        self.assertEqual(
            [group.name for group in groups],
            ["t:zara", "t:media", "t:bare"],
        )
        self.assertEqual(groups[0].label, "Zara")
        self.assertEqual(groups[0].layout, "monadtall")
        self.assertEqual(groups[1].layout, "max")
        self.assertEqual(groups[2].label, "bare")
        self.assertEqual(groups[2].layout, "monadtall")

    def test_generation_is_idempotent_and_registers_names_in_order(self):
        definitions = [("zara", "Zara", "monadtall"), ("web", "Web", "max")]
        build_topic_groups(definitions)
        self.assertEqual(registered_topic_names(), ["t:zara", "t:web"])
        self.assertEqual(build_topic_groups(definitions), [])
        self.assertEqual(registered_topic_names(), ["t:zara", "t:web"])
        self.assertEqual(TOPIC_GROUP_PREFIX, "t:")

    def test_topic_labels_pair_names_with_labels(self):
        qtile = FakeQtile(
            [
                FakeGroup("1", "1", [object()]),
                FakeGroup("t:zara", "Zara", [object()]),
                FakeGroup("t:empty", "Empty"),
            ]
        )
        self.assertEqual(topic_labels(qtile), [["t:zara", "Zara"], ["t:empty", "Empty"]])


class TopicRuntimeTests(unittest.TestCase):
    def setUp(self):
        import qtile_control
        import qtile_topics

        patcher = mock.patch.object(qtile_control, "_notify", lambda *a, **k: None)
        patcher.start()
        self.addCleanup(patcher.stop)
        telemetry = mock.patch.object(
            qtile_topics, "_telemetry_event", lambda *a, **k: None
        )
        telemetry.start()
        self.addCleanup(telemetry.stop)
        TopicGenerationTests._reset_registries()
        self.addCleanup(TopicGenerationTests._reset_registries)
        _install_libqtile_stub()
        self.addCleanup(_remove_libqtile_stub)

    def _fresh_qtile(self):
        return FakeQtile([FakeGroup("1", "1", [object()]), FakeGroup("2", "2")])

    def test_runtime_create_and_remove_topic_commands_work(self):
        qtile = self._fresh_qtile()
        created = create_topic(qtile, "My Topic")
        self.assertEqual(created, "t:my-topic")
        self.assertEqual(
            qtile.added,
            [{"name": "t:my-topic", "layout": "monadtall", "label": "my-topic"}],
        )
        self.assertIn("t:my-topic", registered_topic_names())
        self.assertIn("t:my-topic", qtile.groups_map)
        self.assertEqual(create_topic(qtile, "my-topic"), "t:my-topic")
        self.assertEqual(len(qtile.added), 1)

        self.assertTrue(remove_topic(qtile, "t:my-topic"))
        self.assertEqual(qtile.deleted, ["t:my-topic"])
        self.assertNotIn("t:my-topic", registered_topic_names())
        self.assertFalse(remove_topic(qtile, "t:my-topic"))
        self.assertFalse(remove_topic(qtile, "3"))

    def test_focus_and_send_move_windows_between_topics(self):
        qtile = self._fresh_qtile()
        create_topic(qtile, "zara")
        self.assertTrue(focus_topic(qtile, "zara"))
        self.assertEqual(qtile.groups_map["t:zara"].toscreen_calls, 1)
        self.assertFalse(focus_topic(qtile, "missing"))

        window = FakeWindow(qtile.groups_map["1"])
        qtile.groups_map["1"].windows.append(window)
        qtile.current_window = window
        self.assertTrue(send_current_to_topic(qtile, "t:zara"))
        self.assertEqual(window.moved_to, ("t:zara", True))
        qtile.current_window = None
        self.assertFalse(send_current_to_topic(qtile, "t:zara"))

    def test_next_and_previous_topic_cycle_in_registration_order(self):
        qtile = FakeQtile(
            [
                FakeGroup("1", "1"),
                FakeGroup("t:a", "a"),
                FakeGroup("t:b", "b"),
                FakeGroup("t:c", "c"),
            ]
        )
        qtile.current_group = qtile.groups_map["t:a"]
        self.assertTrue(next_topic(qtile))
        self.assertEqual(qtile.groups_map["t:b"].toscreen_calls, 1)
        self.assertTrue(next_topic(qtile))
        self.assertEqual(qtile.groups_map["t:c"].toscreen_calls, 1)
        self.assertTrue(next_topic(qtile))
        self.assertEqual(qtile.groups_map["t:a"].toscreen_calls, 1)
        self.assertTrue(previous_topic(qtile))
        self.assertEqual(qtile.groups_map["t:c"].toscreen_calls, 2)

    def test_step_topic_without_topics_is_a_noop(self):
        qtile = self._fresh_qtile()
        self.assertFalse(next_topic(qtile))
        self.assertFalse(previous_topic(qtile))

    def test_ipc_exposure_registers_commands_on_the_root_class(self):
        class FakeRoot:
            _commands = {"spawn": lambda self: None}

        exposed = expose_topic_commands(FakeRoot)
        self.assertEqual(
            exposed,
            ["create_topic", "remove_topic", "focus_topic", "send_to_topic"],
        )
        for name in exposed:
            self.assertIn(name, FakeRoot._commands)
            self.assertEqual(FakeRoot._commands[name].__name__, f"ipc_{name}")

    def test_ipc_exposure_survives_a_missing_command_table(self):
        class BareRoot:
            pass

        self.assertEqual(expose_topic_commands(BareRoot), [])


class OwnedGroupBoxTopicVisibilityTests(unittest.TestCase):
    def setUp(self):
        _install_libqtile_stub()
        self.addCleanup(_remove_libqtile_stub)

        hook_mod = types.ModuleType("libqtile.hook")

        class _HookProxy:
            def __getattr__(self, name):
                return lambda *args, **kwargs: None

        hook_mod.subscribe = _HookProxy()
        hook_mod.unsubscribe = _HookProxy()
        sys.modules["libqtile.hook"] = hook_mod

        widget_mod = sys.modules["libqtile.config"]
        libqtile = sys.modules["libqtile"]
        widget_types = types.ModuleType("libqtile.widget")

        class GroupBox:
            def __init__(self, **config):
                self.__dict__.update(config)
                self.visible_groups = config.get("visible_groups")

        widget_types.GroupBox = GroupBox
        libqtile.widget = widget_types
        sys.modules["libqtile.widget"] = widget_types
        self.module = _control_module()

    def tearDown(self):
        sys.modules.pop("libqtile.widget", None)
        sys.modules.pop("libqtile.hook", None)

    def test_topic_groups_appear_and_disappear_with_live_windows(self):
        static = FakeGroup("1", "one", [object()])
        topic = FakeGroup("t:zara", "Zara", [object()])
        empty_topic = FakeGroup("t:web", "Web")
        holding = FakeGroup("hold", "hold")
        scratchpad = FakeGroup("termpad", "term", [object()])
        qtile = FakeQtile([static, topic, empty_topic, holding, scratchpad])

        box = self.module._owned_group_box(
            {
                "colors": [[str(index)] * 2 for index in range(10)],
                "group_names": ["1", "2"],
                "all_group_names": lambda: ["1", "2", "t:zara", "t:web", "hold"],
            }
        )
        box.qtile = qtile
        self.assertEqual(
            [group.name for group in box.groups],
            ["1", "t:zara"],
        )

        topic.windows.clear()
        holding.windows.append(object())
        self.assertEqual([group.name for group in box.groups], ["1", "hold"])

    def test_box_without_all_group_names_falls_back_to_visible_groups(self):
        static = FakeGroup("1", "one", [object()])
        topic = FakeGroup("t:zara", "Zara", [object()])
        qtile = FakeQtile([static, topic])
        box = self.module._owned_group_box(
            {
                "colors": [[str(index)] * 2 for index in range(10)],
                "group_names": ["1"],
            }
        )
        box.qtile = qtile
        self.assertEqual([group.name for group in box.groups], ["1"])


if __name__ == "__main__":
    unittest.main()
