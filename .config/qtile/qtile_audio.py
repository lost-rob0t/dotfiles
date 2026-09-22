#!/usr/bin/env python3
"""Qtile bar audio plugin: active-output volume plus an output picker."""

from __future__ import annotations

import os
import re
import subprocess
from html import escape
from pathlib import Path

from libqtile import popup
from libqtile.widget import base

STATE_DIR = (
    Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state"))
    / "nsa-qtile"
)
STATE_FILE = STATE_DIR / "audio-default-sink"

POLL_SECONDS = 5
VOLUME_STEP = 5
MAX_VOLUME = 150
MENU_WIDTH = 420
ROW_HEIGHT = 28

_ICON_HIGH = "\uf028"
_ICON_LOW = "\uf027"
_ICON_MUTE = "\uf026"
_DEFAULT_MARK = "\u2714 "
_ROW_PAD = "   "

_SINK_SUFFIXES = (
    r"\.analog-stereo",
    r"\.analog-surround(?:\.\d+)?(?:\.\d+)?",
    r"\.iec958-stereo",
    r"\.hdmi(?:\.\d+)*(?:-stereo)?",
    r"\.pro-audio",
    r"\.stereo",
)


def run_pactl(*arguments: str, timeout: int = 5) -> str:
    """Run pactl with ARGUMENTS; return stdout, or '' when it fails."""
    try:
        result = subprocess.run(
            ["pactl", *arguments],
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except (OSError, subprocess.TimeoutExpired):
        return ""
    return result.stdout if result.returncode == 0 else ""


def parse_sinks(text: str) -> list[dict]:
    """Parse `pactl list sinks` output into sink dictionaries."""
    sinks = []
    for block in text.split("Sink #")[1:]:
        name = re.search(r"^\s*Name:\s*(\S+)", block, re.MULTILINE)
        description = re.search(
            r"^\s*Description:\s*(.*)$", block, re.MULTILINE
        )
        mute = re.search(r"^\s*Mute:\s*(\w+)", block, re.MULTILINE)
        if name:
            sinks.append(
                {
                    "name": name.group(1),
                    "description": (
                        description.group(1).strip()
                        if description
                        else name.group(1)
                    ),
                    "mute": bool(mute and mute.group(1) == "yes"),
                }
            )
    return sinks


def parse_volume_percent(text: str) -> int:
    """Return the first channel percentage from get-sink-volume output."""
    match = re.search(r"(\d+)%", text)
    return int(match.group(1)) if match else 0


def clamp_percent(value: int) -> int:
    """Clamp a volume percent into the supported range."""
    return max(0, min(MAX_VOLUME, value))


def short_sink_name(name: str) -> str:
    """Strip ALSA/BlueZ naming noise from a sink name."""
    short = name
    for prefix in ("alsa_output.", "bluez_output.", "alsa_input."):
        if short.startswith(prefix):
            short = short[len(prefix) :]
    for suffix in _SINK_SUFFIXES:
        short = re.sub(suffix + r"$", "", short)
    return short or name


def icon_for(volume: int, mute: bool) -> str:
    """Pick the nerd-font glyph for the current state."""
    if mute or volume == 0:
        return _ICON_MUTE
    return _ICON_HIGH if volume >= 40 else _ICON_LOW


def sink_label(sink: dict, default_name: str) -> str:
    """One dropdown row; the active sink is marked and escaped."""
    mark = _DEFAULT_MARK if sink["name"] == default_name else _ROW_PAD
    return mark + escape(sink["description"])


def read_preferred_sink() -> str:
    """Return the pinned preferred default sink, if any."""
    try:
        return STATE_FILE.read_text(encoding="utf-8").strip()
    except OSError:
        return ""


def write_preferred_sink(name: str) -> None:
    """Pin NAME as the preferred default sink for future sessions."""
    try:
        STATE_DIR.mkdir(parents=True, exist_ok=True)
        STATE_FILE.write_text(name + "\n", encoding="utf-8")
    except OSError:
        pass


class SinkMenu(popup.Popup):
    """Dropdown of audio outputs; a row click selects the default sink."""

    def __init__(self, qtile, sinks, default_name, on_select, **config):
        self._sinks = sinks
        self._default_name = default_name
        self.on_select = on_select
        self._row_height = ROW_HEIGHT
        padding = config.get("vertical_padding", 0)
        super().__init__(
            qtile,
            width=MENU_WIDTH,
            height=len(sinks) * ROW_HEIGHT + 2 * padding,
            **config,
        )
        self._draw_rows()

    def _draw_rows(self):
        pad = self.horizontal_padding
        for index, sink in enumerate(self._sinks):
            self.layout.text = sink_label(sink, self._default_name)
            self.layout.width = self.width - pad * 2
            self.layout.draw(pad, self.vertical_padding + index * self._row_height)
        self.place()
        self.draw()
        self.unhide()

    def process_button_click(self, x, y, button):
        self.hide()
        if button != 1 or not self._sinks:
            return
        index = (y - self.vertical_padding) // self._row_height
        if 0 <= index < len(self._sinks):
            self.on_select(self._sinks[index])


class Audio(base.BackgroundPoll):
    """Bar widget controlling the active output through pactl."""

    defaults = [
        (
            "update_interval",
            POLL_SECONDS,
            "Seconds between sink/volume refreshes.",
        ),
    ]

    def __init__(self, **config):
        base.BackgroundPoll.__init__(self, **config)
        self.add_defaults(Audio.defaults)
        self._sinks: list[dict] = []
        self._default = ""
        self._volume = 0
        self._mute = False
        self._menu = None
        self._applied_preference = False
        self.mouse_callbacks = {
            "Button1": self.show_sink_menu,
            "Button2": self.toggle_mute,
            "Button4": lambda: self.change_volume(VOLUME_STEP),
            "Button5": lambda: self.change_volume(-VOLUME_STEP),
        }

    def poll(self):
        """Refresh sink state; runs off the event loop. Returns bar text."""
        self._sinks = parse_sinks(run_pactl("list", "sinks"))
        self._default = run_pactl("get-default-sink").strip()
        if not self._default and self._sinks:
            self._default = self._sinks[0]["name"]
        volume_text = run_pactl("get-sink-volume", "@DEFAULT_SINK@")
        self._volume = parse_volume_percent(volume_text)
        self._mute = "mute: yes" in run_pactl(
            "get-sink-mute", "@DEFAULT_SINK@"
        ).lower()
        if not self._applied_preference:
            self._applied_preference = True
            self._apply_saved_preference()
        if not self._default:
            return "\uf026  no audio"
        return (
            f"{icon_for(self._volume, self._mute)} "
            f"{self._volume}%  {short_sink_name(self._default)}"
        )

    def _apply_saved_preference(self):
        preferred = read_preferred_sink()
        if (
            preferred
            and preferred != self._default
            and any(sink["name"] == preferred for sink in self._sinks)
        ):
            run_pactl("set-default-sink", preferred)
            self._default = preferred

    def _set_default(self, name):
        run_pactl("set-default-sink", name)
        self._default = name
        write_preferred_sink(name)

    def _refresh_sync(self):
        """Poll again and push the new text onto the event loop."""
        self.qtile.call_soon_threadsafe(self.update, self.poll() or "")

    def _spawn(self, function, *arguments):
        self.qtile.run_in_executor(function, *arguments)

    def change_volume(self, step):
        """Raise/lower the active sink by STEP percent, off-loop."""
        def action():
            run_pactl(
                "set-sink-volume",
                "@DEFAULT_SINK@",
                f"{clamp_percent(self._volume + step)}%",
            )
            self._refresh_sync()

        self._spawn(action)

    def toggle_mute(self):
        """Toggle mute on the active sink, off-loop."""
        def action():
            run_pactl("set-sink-mute", "@DEFAULT_SINK@", "toggle")
            self._refresh_sync()

        self._spawn(action)

    def _select_sink(self, sink):
        def action():
            self._set_default(sink["name"])
            self._refresh_sync()

        self._spawn(action)

    def show_sink_menu(self):
        """Open the output dropdown; uses the state cached by poll()."""
        if not self._sinks:
            return
        if self._menu is not None:
            self._menu.hide()
        menu = SinkMenu(
            self.qtile,
            self._sinks,
            self._default,
            self._select_sink,
            x=(self.bar.screen.width - MENU_WIDTH) // 2,
            y=self.bar.height + 8,
            background=self.background,
            foreground=self.foreground,
            font=self.font,
            fontsize=self.fontsize + 2,
            border=self.foreground,
            border_width=1,
            horizontal_padding=12,
            vertical_padding=10,
        )
        self._menu = menu
