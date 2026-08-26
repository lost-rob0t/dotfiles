#!/usr/bin/env python3
"""Behavioral tests for root filesystem and disk telemetry."""

from __future__ import annotations

import importlib.util
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest import mock

SOURCE = Path(__file__).resolve().parents[1] / "qtile_system.py"
sys.path.insert(0, str(SOURCE.parent))
WIDGET_MODULE = None


def _install_libqtile_stub():
    global WIDGET_MODULE
    libqtile = types.ModuleType("libqtile")
    widget_mod = types.ModuleType("libqtile.widget")
    base_mod = types.ModuleType("libqtile.widget.base")

    class Widget:
        def __init__(self, width=0, **config):
            self.width = width
            self.height = config.get("height", 26)
            self.mouse_callbacks = config.get("mouse_callbacks", {})
            self.__dict__.update(config)

        def add_defaults(self, defaults):
            for name, value, _description in defaults:
                if not hasattr(self, name):
                    setattr(self, name, value)

        def add_callbacks(self, defaults):
            defaults.update(self.mouse_callbacks)
            self.mouse_callbacks = defaults

    class BackgroundPoll(Widget):
        pass

    base_mod._Widget = Widget
    base_mod.BackgroundPoll = BackgroundPoll
    base_mod.ORIENTATION_HORIZONTAL = 1
    widget_mod.base = base_mod
    WIDGET_MODULE = widget_mod
    libqtile.widget = widget_mod
    sys.modules["libqtile"] = libqtile
    sys.modules["libqtile.widget"] = widget_mod
    sys.modules["libqtile.widget.base"] = base_mod


_install_libqtile_stub()
SPEC = importlib.util.spec_from_file_location("qtile_system", SOURCE)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules[SPEC.name] = MODULE
SPEC.loader.exec_module(MODULE)


class Counter:
    def __init__(self, read_bytes, write_bytes):
        self.read_bytes = read_bytes
        self.write_bytes = write_bytes


class Partition:
    device = "/dev/disk42p7"
    mountpoint = "/"


class QtileSystemTests(unittest.TestCase):
    def test_binary_formatting_and_root_free(self):
        self.assertEqual(MODULE.format_binary_bytes(1024**3), "1GiB")
        with mock.patch.object(MODULE.psutil, "disk_usage", return_value=types.SimpleNamespace(free=182 * 1024**3)):
            self.assertEqual(MODULE.root_free_text(), "182GiB free")

    def test_root_device_resolution_includes_partition_parent_and_slaves(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            physical = root / "devices" / "disk42"
            partition = physical / "disk42p7"
            mapper = root / "devices" / "layer0"
            partition.mkdir(parents=True)
            mapper.mkdir(parents=True)
            (partition / "partition").touch()
            (partition / "slaves").mkdir()
            (partition / "slaves" / "layer0").mkdir()
            (mapper / "slaves").mkdir()
            (mapper / "slaves" / "disk42p7").mkdir()
            (root / "disk42").symlink_to(physical, target_is_directory=True)
            (root / "disk42p7").symlink_to(partition, target_is_directory=True)
            (root / "layer0").symlink_to(mapper, target_is_directory=True)
            names = MODULE.root_device_names("/dev/disk42p7", sys_class_block=root)
        self.assertEqual(names, {"disk42p7", "disk42", "layer0"})

    def test_partition_parent_uses_sysfs_for_arbitrary_device_names(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            parent = root / "devices" / "arbitrary-parent"
            entry = parent / "disk42"
            entry.mkdir(parents=True)
            (entry / "partition").touch()
            (root / "disk42").symlink_to(entry, target_is_directory=True)
            self.assertEqual(MODULE._partition_parent("disk42", root), "arbitrary-parent")

    def test_root_totals_resolve_partition_and_mapper_counters(self):
        counters = {
            "disk42p7": Counter(10, 20),
            "disk42": Counter(100, 200),
            "layer0": Counter(5, 6),
            "sdb": Counter(1000, 1000),
        }
        with mock.patch.object(MODULE, "_partition_device", return_value="/dev/disk42p7"), mock.patch.object(
            MODULE, "root_device_names", return_value={"disk42p7", "disk42", "layer0"}
        ):
            self.assertEqual(MODULE.root_io_totals(counters), (10, 20))

    def test_mapper_totals_do_not_double_count_partition_and_parent(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "layer0" / "slaves").mkdir(parents=True)
            (root / "layer0" / "slaves" / "disk42p7").mkdir()
            counters = {
                "layer0": Counter(10, 10),
                "disk42": Counter(100, 100),
                "disk42p7": Counter(20, 30),
            }
            totals = MODULE.root_io_totals(
                counters,
                device="/dev/layer0",
                sys_class_block=root,
            )
        self.assertEqual(totals, (20, 30))

    def test_sampler_handles_initial_sample_elapsed_time_and_counter_reset(self):
        sampler = MODULE.DiskIOSampler(samples=4)
        with mock.patch.object(MODULE, "root_io_totals", side_effect=[(100, 50), (300, 150), (10, 5), (20, 15)]):
            self.assertIsNone(sampler.sample(now=10.0))
            self.assertEqual(sampler.sample(now=12.0), (100.0, 50.0))
            self.assertEqual(sampler.sample(now=13.0), (0.0, 0.0))
            self.assertEqual(sampler.sample(now=15.0), (5.0, 5.0))
        self.assertTrue(all(value >= 0 for value in (*sampler.read, *sampler.write)))

    def test_sampler_handles_missing_and_zero_elapsed_counters(self):
        sampler = MODULE.DiskIOSampler()
        with mock.patch.object(MODULE, "root_io_totals", side_effect=[None, (10, 10), (20, 20)]):
            self.assertIsNone(sampler.sample(now=1.0))
            self.assertIsNone(sampler.sample(now=2.0))
            self.assertIsNone(sampler.sample(now=2.0))

    def test_fixed_icon_cell_reserves_width(self):
        _install_libqtile_stub()
        WIDGET_MODULE.TextBox = lambda **config: config
        cell = MODULE.telemetry_icon_cell("CPU", "cyan", "background", name="cpu")
        self.assertEqual(cell["width"], 24)
        self.assertEqual(cell["padding"], 0)
        disk = MODULE.telemetry_icon_cell("DISK", "orange", "background", width=18)
        self.assertEqual(disk["width"], 18)
        compact = MODULE.telemetry_icon_cell("RAM", "green", "background", width=None)
        self.assertNotIn("width", compact)


if __name__ == "__main__":
    unittest.main()
