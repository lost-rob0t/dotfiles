#!/usr/bin/env python3
"""Tests for the qtile_audio plugin: structure, parsers, and parity."""

from __future__ import annotations

import ast
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
SOURCE = HERE.parent / "qtile_audio.py"
SOURCE_TEXT = SOURCE.read_text(encoding="utf-8")
TREE = ast.parse(SOURCE_TEXT)
ORG = HERE.parent / "qtile-audio.org"

# /usr/bin/python carries the system libqtile that the live bar uses.
SYSTEM_PYTHON = Path("/usr/bin/python")

def _system_python_has_libqtile() -> bool:
    """True when SYSTEM_PYTHON can import libqtile (the live bar's env)."""
    if not SYSTEM_PYTHON.exists():
        return False
    probe = subprocess.run(
        [str(SYSTEM_PYTHON), "-c", "import libqtile"],
        capture_output=True,
        timeout=30,
    )
    return probe.returncode == 0


HAS_LIBQTILE = _system_python_has_libqtile()


RUNNER = r"""
import json, sys
sys.path.insert(0, {source_dir!r})
import qtile_audio as qa
cases = json.load(sys.stdin)
results = {{}}
results["sinks"] = qa.parse_sinks(cases["sinks"])
results["volume"] = qa.parse_volume_percent(cases["volume_text"])
results["clamped_low"] = qa.clamp_percent(-3)
results["clamped_high"] = qa.clamp_percent(qa.MAX_VOLUME + 20)
results["short"] = qa.short_sink_name(cases["sink_name"])
results["label_default"] = qa.sink_label(cases["sinks_parsed"][0], cases["sinks_parsed"][0]["name"])
results["label_other"] = qa.sink_label(cases["sinks_parsed"][1], cases["sinks_parsed"][0]["name"])
results["icon_mute"] = qa.icon_for(80, True)
results["icon_zero"] = qa.icon_for(0, False)
results["icon_low"] = qa.icon_for(20, False)
results["icon_high"] = qa.icon_for(80, False)
results["preferred"] = qa.read_preferred_sink()
qa.write_preferred_sink("alsa_output.usb-test")
results["written"] = qa.read_preferred_sink()
print(json.dumps(results))
"""


def assigned_constant(name):
    for node in TREE.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == name:
                    return ast.literal_eval(node.value)
    raise AssertionError(f"missing constant {name}")


def function_source(name):
    for node in TREE.body:
        if isinstance(node, ast.FunctionDef) and node.name == name:
            return ast.get_source_segment(SOURCE_TEXT, node) or ""
    raise AssertionError(f"missing function {name}")


def method_source(class_name, method_name):
    for node in TREE.body:
        if isinstance(node, ast.ClassDef) and node.name == class_name:
            for child in node.body:
                if (
                    isinstance(child, ast.FunctionDef)
                    and child.name == method_name
                ):
                    return (
                        ast.get_source_segment(SOURCE_TEXT, child) or ""
                    )
    raise AssertionError(f"missing method {class_name}.{method_name}")


class QtileAudioStructureTests(unittest.TestCase):
    def test_widget_and_menu_classes_exist(self):
        classes = {node.name for node in TREE.body if isinstance(node, ast.ClassDef)}
        self.assertIn("Audio", classes)
        self.assertIn("SinkMenu", classes)

    def test_polling_is_threaded_not_in_loop(self):
        audio_bases = next(
            node
            for node in TREE.body
            if isinstance(node, ast.ClassDef) and node.name == "Audio"
        ).bases
        self.assertEqual(
            [ast.unparse(base) for base in audio_bases], ["base.BackgroundPoll"]
        )
        self.assertEqual(assigned_constant("POLL_SECONDS"), 5)

    def test_volume_bounds_and_step_are_bounded(self):
        self.assertEqual(assigned_constant("VOLUME_STEP"), 5)
        self.assertEqual(assigned_constant("MAX_VOLUME"), 150)
        self.assertIn("clamp_percent", function_source("clamp_percent"))

    def test_all_pactl_calls_use_default_sink_target(self):
        self.assertIn('"@DEFAULT_SINK@"', method_source("Audio", "toggle_mute"))
        self.assertIn('"@DEFAULT_SINK@"', method_source("Audio", "change_volume"))
        self.assertIn("set-default-sink", SOURCE_TEXT)
        self.assertNotIn("set Master", SOURCE_TEXT)

    def test_dropdown_rows_are_clickable_and_escaped(self):
        self.assertIn("process_button_click", SOURCE_TEXT)
        self.assertIn("self.on_select", method_source("SinkMenu", "process_button_click"))
        self.assertIn("escape(", function_source("sink_label"))

    def test_default_output_is_persisted_and_restored(self):
        self.assertIn("write_preferred_sink", SOURCE_TEXT)
        self.assertIn("read_preferred_sink", SOURCE_TEXT)
        self.assertIn("_apply_saved_preference", method_source("Audio", "poll"))

    def test_widget_uses_mouse_callbacks(self):
        init_source = method_source("Audio", "__init__")
        for button in ("Button1", "Button2", "Button4", "Button5"):
            self.assertIn(f'"{button}"', init_source)

    def test_no_blocking_subprocess_in_click_paths(self):
        for method in ("change_volume", "toggle_mute", "_select_sink"):
            self.assertIn("_spawn", method_source("Audio", method))
        self.assertNotIn("run_pactl(", method_source("Audio", "show_sink_menu"))

    def test_literate_source_matches_runtime(self):
        lines = ORG.read_text(encoding="utf-8").splitlines()
        start = lines.index("#+begin_src python") + 1
        end = lines.index("#+end_src", start)
        self.assertEqual("\n".join(lines[start:end]) + "\n", SOURCE_TEXT)


@unittest.skipUnless(
    HAS_LIBQTILE,
    "system python with libqtile not available",
)
class QtileAudioBehaviorTests(unittest.TestCase):
    def run_cases(self, cases):
        state_home = cases.pop("XDG_STATE_HOME")
        environment = dict(os.environ, XDG_STATE_HOME=state_home)
        completed = subprocess.run(
            [str(SYSTEM_PYTHON), "-c", RUNNER.format(source_dir=str(HERE.parent))],
            input=json.dumps(cases),
            capture_output=True,
            text=True,
            timeout=30,
            env=environment,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        return json.loads(completed.stdout)

    def test_parsers_and_state_roundtrip(self):
        sinks_text = (
            "Sink #0\n"
            "\tState: RUNNING\n"
            "\tName: alsa_output.pci-0000_0a_00.1.analog-stereo\n"
            "\tDescription: Built-in Audio Analog Stereo\n"
            "\tMute: no\n"
            "Sink #1\n"
            "\tState: IDLE\n"
            "\tName: bluez_output.XX_11_22_33_44_55.1\n"
            "\tDescription: Headphones & Mic (A2DP)\n"
            "\tMute: yes\n"
        )
        parsed = [
            {
                "name": "alsa_output.pci-0000_0a_00.1.analog-stereo",
                "description": "Built-in Audio Analog Stereo",
                "mute": False,
            },
            {
                "name": "bluez_output.XX_11_22_33_44_55.1",
                "description": "Headphones & Mic (A2DP)",
                "mute": True,
            },
        ]
        with tempfile.TemporaryDirectory() as state_home:
            results = self.run_cases(
                {
                    "sinks": sinks_text,
                    "volume_text": "Volume: front-left: 52428 /  80% / -5.81 dB",
                    "sink_name": "alsa_output.pci-0000_0a_00.1.analog-stereo",
                    "sinks_parsed": parsed,
                    "XDG_STATE_HOME": state_home,
                }
            )
        self.assertEqual(len(results["sinks"]), 2)
        self.assertEqual(
            results["sinks"][1]["description"], "Headphones & Mic (A2DP)"
        )
        self.assertTrue(results["sinks"][1]["mute"])
        self.assertEqual(results["volume"], 80)
        self.assertEqual(results["clamped_low"], 0)
        self.assertEqual(results["clamped_high"], 150)
        self.assertEqual(results["short"], "pci-0000_0a_00.1")
        self.assertTrue(results["label_default"].startswith("\u2714 "))
        self.assertTrue(results["label_other"].startswith("   "))
        self.assertIn("&amp;", results["label_other"])
        self.assertNotIn("&", results["label_other"].replace("&amp;", ""))
        self.assertEqual(results["icon_mute"], results["icon_zero"])
        self.assertNotEqual(results["icon_low"], results["icon_high"])
        self.assertEqual(results["written"], "alsa_output.usb-test")


if __name__ == "__main__":
    unittest.main()
