"""Combined truthful download/upload graph for the Qtile system bar."""

from __future__ import annotations

import time
from collections import deque
from pathlib import Path

from libqtile.widget import base


class NetIOGraph(base._Widget):
    """Draw download and upload rates together on one shared linear scale.

    Cyan/blue is incoming/download traffic. Pink/red is outgoing/upload traffic.
    Both series use the same ceiling so relative magnitude stays truthful.
    """

    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ("frequency", 1.0, "Refresh interval in seconds."),
        ("samples", 60, "Number of samples retained."),
        ("download_color", "#00b8ff", "Incoming/download line color."),
        ("upload_color", "#f6019d", "Outgoing/upload line color."),
        ("line_width", 1.2, "Line width."),
        ("margin_x", 2, "Horizontal margin."),
        ("margin_y", 2, "Vertical margin."),
    ]

    def __init__(self, width=64, **config):
        super().__init__(width, **config)
        self.add_defaults(self.defaults)
        self.download = deque(maxlen=max(int(self.samples), 2))
        self.upload = deque(maxlen=max(int(self.samples), 2))
        self._last = None
        self._last_at = None

    @staticmethod
    def _default_interface() -> str | None:
        """Return the interface carrying the default IPv4 route."""
        try:
            lines = Path("/proc/net/route").read_text(encoding="utf-8").splitlines()[1:]
        except OSError:
            return None
        for line in lines:
            fields = line.split()
            if len(fields) < 4:
                continue
            interface, destination, _gateway, flags = fields[:4]
            try:
                route_flags = int(flags, 16)
            except ValueError:
                continue
            if destination == "00000000" and route_flags & 0x1:
                return interface
        return None

    @classmethod
    def _read_bytes(cls) -> tuple[int, int] | None:
        interface = cls._default_interface()
        try:
            lines = Path("/proc/net/dev").read_text(encoding="utf-8").splitlines()[2:]
        except OSError:
            return None
        fallback = None
        for line in lines:
            name, _, data = line.partition(":")
            name = name.strip()
            fields = data.split()
            if len(fields) < 16 or name == "lo":
                continue
            try:
                rx, tx = int(fields[0]), int(fields[8])
            except ValueError:
                continue
            if name == interface:
                return rx, tx
            if fallback is None or rx + tx > fallback[0] + fallback[1]:
                fallback = (rx, tx)
        return fallback

    def timer_setup(self):
        self._update()
        self.timeout_add(self.frequency, self.timer_setup)

    def _update(self):
        now = time.monotonic()
        current = self._read_bytes()
        if current is not None and self._last is not None and self._last_at is not None:
            elapsed = max(now - self._last_at, 0.001)
            down = max((current[0] - self._last[0]) / elapsed, 0.0)
            up = max((current[1] - self._last[1]) / elapsed, 0.0)
            self.download.append(down)
            self.upload.append(up)
            self.draw()
        if current is not None:
            self._last = current
            self._last_at = now

    def _draw_series(self, values, color, ceiling, usable_height):
        if not values or ceiling <= 0:
            return
        count = max(len(values) - 1, 1)
        usable_width = max(self.width - 2 * self.margin_x, 1)
        baseline = self.height - self.margin_y
        self.drawer.set_source_rgb(color)
        self.drawer.ctx.set_line_width(self.line_width)
        for index, value in enumerate(values):
            x = self.margin_x + (index / count) * usable_width
            y = baseline - min(float(value) / ceiling, 1.0) * usable_height
            if index == 0:
                self.drawer.ctx.move_to(x, y)
            else:
                self.drawer.ctx.line_to(x, y)
        self.drawer.ctx.stroke()

    def draw(self):
        self.drawer.clear(self.background or self.bar.background)
        ceiling = max([*self.download, *self.upload, 0.0])
        usable_height = max(self.height - 2 * self.margin_y, 1)
        self._draw_series(self.download, self.download_color, ceiling, usable_height)
        self._draw_series(self.upload, self.upload_color, ceiling, usable_height)
        self.draw_at_default_position()
