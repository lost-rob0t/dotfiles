"""Small, non-blocking system telemetry widgets for the Qtile bar."""

from __future__ import annotations

import os
import time
from collections import deque
from pathlib import Path
from typing import Any, Iterable

import psutil
from libqtile.widget import base


def format_binary_bytes(value: int | float | None) -> str:
    """Format bytes with binary units while keeping the bar compact."""
    amount = max(float(value or 0), 0.0)
    units = ("B", "KiB", "MiB", "GiB", "TiB", "PiB")
    unit = units[0]
    for candidate in units:
        unit = candidate
        if amount < 1024 or candidate == units[-1]:
            break
        amount /= 1024
    if unit == "B":
        return f"{int(amount)}B"
    return f"{amount:.1f}{unit}".replace(".0", "")


def root_free_text(path: str = "/") -> str:
    """Return free space for the filesystem mounted at ``/``."""
    try:
        free = psutil.disk_usage(path).free
    except (OSError, AttributeError, psutil.Error):
        return "? free"
    return f"{format_binary_bytes(free)} free"


def _partition_device() -> str | None:
    try:
        partitions = psutil.disk_partitions(all=True)
    except (OSError, AttributeError, psutil.Error):
        return None
    for partition in partitions:
        if getattr(partition, "mountpoint", None) == "/":
            return str(getattr(partition, "device", "")) or None
    return None


def _partition_parent(name: str, sys_class_block: Path = Path("/sys/class/block")) -> str | None:
    """Return a partition's parent from sysfs, independent of device naming."""
    entry = sys_class_block / name
    if not (entry / "partition").exists():
        return None
    try:
        parent = entry.resolve().parent.name
    except OSError:
        return None
    return parent if parent and parent != name else None
    return None


def root_device_names(device: str | None = None, *, sys_class_block: Path = Path("/sys/class/block")) -> set[str]:
    """Resolve root, mapper, and physical slave device names for psutil."""
    initial = device or _partition_device()
    if not initial:
        return set()
    names: set[str] = set()
    pending = [Path(initial).name, Path(os.path.realpath(initial)).name]
    while pending:
        name = pending.pop()
        if not name or name in names:
            continue
        names.add(name)
        parent = _partition_parent(name, sys_class_block)
        if parent:
            pending.append(parent)
        slaves = sys_class_block / name / "slaves"
        try:
            pending.extend(child.name for child in slaves.iterdir())
        except OSError:
            pass
    return names


def _counter_bytes(counter: Any, field: str) -> int:
    try:
        return max(int(getattr(counter, field, 0) or 0), 0)
    except (TypeError, ValueError):
        return 0


def _slave_names(name: str, sys_class_block: Path) -> set[str]:
    try:
        return {child.name for child in (sys_class_block / name / "slaves").iterdir()}
    except OSError:
        return set()


def _leaf_counter_names(names: set[str], counters: dict[str, Any], sys_class_block: Path) -> set[str]:
    selected = {
        name for name in names if name in counters and not _slave_names(name, sys_class_block)
    }
    # A partition and its whole-disk parent have separate psutil entries but
    # describe the same I/O. Prefer the partition when both are available.
    for name in tuple(selected):
        parent = _partition_parent(name, sys_class_block)
        if parent in selected:
            selected.remove(parent)
    return selected


def root_io_totals(
    counters: dict[str, Any] | None = None,
    *,
    device: str | None = None,
    sys_class_block: Path = Path("/sys/class/block"),
) -> tuple[int, int] | None:
    """Sum read/write counters for the root device and its backing slaves."""
    try:
        counters = counters if counters is not None else psutil.disk_io_counters(perdisk=True)
    except (OSError, AttributeError, psutil.Error):
        return None
    if not counters:
        return None
    configured_device = device or _partition_device()
    names = root_device_names(configured_device, sys_class_block=sys_class_block)
    selected_names: set[str] = set()
    for candidate in (
        Path(configured_device).name if configured_device else "",
        Path(os.path.realpath(configured_device)).name if configured_device else "",
    ):
        if candidate not in counters:
            continue
        slaves = _slave_names(candidate, sys_class_block)
        selected_names = (
            _leaf_counter_names(names, counters, sys_class_block)
            if slaves
            else {candidate}
        )
        break
    if not selected_names:
        parent = (
            _partition_parent(Path(configured_device).name, sys_class_block)
            if configured_device
            else None
        )
        if parent and parent in counters:
            selected_names = {parent}
        else:
            selected_names = _leaf_counter_names(names, counters, sys_class_block)
    selected = [counter for name, counter in counters.items() if name in selected_names]
    if not selected:
        return None
    return (
        sum(_counter_bytes(counter, "read_bytes") for counter in selected),
        sum(_counter_bytes(counter, "write_bytes") for counter in selected),
    )


class DiskIOSampler:
    """Calculate reset-safe read/write rates from root disk counters."""

    def __init__(self, samples: int = 60):
        self.read = deque(maxlen=max(int(samples), 2))
        self.write = deque(maxlen=max(int(samples), 2))
        self._last: tuple[int, int] | None = None
        self._last_at: float | None = None

    def sample(
        self,
        counters: dict[str, Any] | None = None,
        *,
        now: float | None = None,
    ) -> tuple[float, float] | None:
        current = root_io_totals(counters)
        stamp = time.monotonic() if now is None else float(now)
        if current is None:
            self._last = None
            self._last_at = None
            return None
        result = None
        if self._last is not None and self._last_at is not None:
            elapsed = stamp - self._last_at
            if elapsed > 0:
                result = (
                    max((current[0] - self._last[0]) / elapsed, 0.0),
                    max((current[1] - self._last[1]) / elapsed, 0.0),
                )
                self.read.append(result[0])
                self.write.append(result[1])
        self._last = current
        self._last_at = stamp
        return result


def telemetry_icon_cell(
    icon: str,
    color: str,
    background: str,
    *,
    name: str | None = None,
    width: int = 24,
):
    """Build a fixed-width icon cell so glyph bearings cannot clip graphs."""
    from libqtile import widget

    config = {
        "text": icon,
        "font": "Hack Nerd Regular",
        "fontsize": 13,
        "foreground": color,
        "background": background,
        "padding": 0,
        "width": width,
    }
    if name:
        config["name"] = name
    return widget.TextBox(**config)


class RootFree(base.BackgroundPoll):
    """Display free space on the root filesystem without spawning df."""

    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, **config):
        super().__init__(text="? free", **config)

    def poll(self):
        return root_free_text()


class DiskIOGraph(base._Widget):
    """Draw reset-safe root read/write rates on one shared scale."""

    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ("frequency", 1.0, "Refresh interval in seconds."),
        ("samples", 60, "Number of samples retained."),
        ("read_color", "#00b8ff", "Read-rate line color."),
        ("write_color", "#f6019d", "Write-rate line color."),
        ("line_width", 1.2, "Line width."),
        ("margin_x", 2, "Horizontal margin."),
        ("margin_y", 2, "Vertical margin."),
    ]

    def __init__(self, width=64, **config):
        super().__init__(width, **config)
        self.add_defaults(self.defaults)
        self.sampler = DiskIOSampler(self.samples)

    def timer_setup(self):
        self._update()
        self.timeout_add(self.frequency, self.timer_setup)

    def _update(self):
        if self.sampler.sample() is not None:
            self.draw()

    def _draw_series(self, values: Iterable[float], color: str, ceiling: float, height: float):
        values = list(values)
        if not values or ceiling <= 0:
            return
        count = max(len(values) - 1, 1)
        usable_width = max(self.width - 2 * self.margin_x, 1)
        baseline = self.height - self.margin_y
        self.drawer.set_source_rgb(color)
        self.drawer.ctx.set_line_width(self.line_width)
        for index, value in enumerate(values):
            x = self.margin_x + index / count * usable_width
            y = baseline - min(float(value) / ceiling, 1.0) * height
            if index == 0:
                self.drawer.ctx.move_to(x, y)
            else:
                self.drawer.ctx.line_to(x, y)
        self.drawer.ctx.stroke()

    def draw(self):
        self.drawer.clear(self.background or self.bar.background)
        ceiling = max([*self.sampler.read, *self.sampler.write, 0.0])
        height = max(self.height - 2 * self.margin_y, 1)
        self._draw_series(self.sampler.read, self.read_color, ceiling, height)
        self._draw_series(self.sampler.write, self.write_color, ceiling, height)
        self.draw_at_default_position()
