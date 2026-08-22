"""OpenRouter telemetry widgets and small Qtile runtime helpers."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import threading
import time
from collections import deque
from pathlib import Path

from libqtile.config import Key
from libqtile.lazy import lazy
from libqtile.widget import base

POLL_SECONDS = 5
GRAPH_SAMPLES = 60
GRAPH_WIDTH = 76
RATE_FONTSIZE = 12

_poll_lock = threading.Lock()
_last_poll_at = 0.0
_last_payload = None
_sync_lock = threading.Lock()
_sync_in_progress = False


def _color(colors, index):
    value = colors[index]
    if isinstance(value, (list, tuple)):
        return value[0]
    return value


def _cache_path():
    root = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser()
    return root / "qtile" / "openrouter-status.json"


def _read_cached_status():
    try:
        cache = json.loads(_cache_path().read_text(encoding="utf-8"))
        return cache.get("status")
    except (FileNotFoundError, json.JSONDecodeError, OSError, TypeError):
        return None


def _compact_count(value):
    value = float(value or 0)
    for suffix, divisor in (("B", 1_000_000_000), ("M", 1_000_000), ("k", 1_000)):
        if abs(value) >= divisor:
            rendered = f"{value / divisor:.1f}".rstrip("0").rstrip(".")
            return f"{rendered}{suffix}"
    return str(int(round(value)))


def _fetch_payload(script):
    global _last_payload, _last_poll_at

    with _poll_lock:
        now = time.monotonic()
        if _last_payload is not None and now - _last_poll_at < POLL_SECONDS - 0.5:
            return _last_payload

        completed = subprocess.run(
            ["python3", script, "--json"],
            check=False,
            capture_output=True,
            text=True,
            timeout=8,
        )
        try:
            payload = json.loads(completed.stdout)
        except json.JSONDecodeError:
            payload = None

        _last_poll_at = time.monotonic()
        if isinstance(payload, dict) and "input_tokens_per_minute" in payload:
            _last_payload = payload
            return payload

        cached = _read_cached_status()
        if cached:
            payload = dict(cached)
            payload["stale"] = True
            _last_payload = payload
            return payload

        return None


def _notify(summary, body="", urgency="normal"):
    notifier = shutil.which("dunstify") or shutil.which("notify-send")
    if not notifier:
        return

    command = [notifier, "-a", "Qtile", "-u", urgency, summary]
    if body:
        command.append(body)
    try:
        subprocess.Popen(
            command,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    except OSError:
        pass


def _git_sync_command():
    helper = Path.home() / ".config" / "bash" / "git-sync.sh"
    repo = Path.home() / ".dotfiles"
    return (
        f'source "{helper}" && '
        f'git-sync "{repo}"'
    )


def _run_dotfiles_sync():
    return subprocess.run(
        ["bash", "-lc", _git_sync_command()],
        check=False,
        capture_output=True,
        text=True,
        timeout=120,
    )


def _sync_and_reload(qtile):
    """Sync dotfiles off the event loop, then reload Qtile in-process."""
    global _sync_in_progress

    with _sync_lock:
        if _sync_in_progress:
            _notify("Dotfiles sync", "Already syncing.", "low")
            return
        _sync_in_progress = True

    _notify("Dotfiles sync", "Fetching updates…")

    def worker():
        global _sync_in_progress
        try:
            completed = _run_dotfiles_sync()
        except (OSError, subprocess.SubprocessError) as error:
            _notify("Dotfiles sync failed", str(error), "critical")
            with _sync_lock:
                _sync_in_progress = False
            return

        with _sync_lock:
            _sync_in_progress = False

        if completed.returncode != 0:
            detail = (completed.stderr or completed.stdout or "git-sync failed").strip()
            _notify("Dotfiles sync failed", detail[-800:], "critical")
            return

        detail = (completed.stdout or "Dotfiles are up to date.").strip()
        _notify("Dotfiles synced", detail[-400:] + "\nReloading Qtile…")
        qtile.call_soon_threadsafe(qtile.reload_config)

    threading.Thread(
        target=worker,
        name="qtile-dotfiles-sync",
        daemon=True,
    ).start()


class OpenRouterRate(base.BackgroundPoll):
    """Display a rolling 60-second OpenRouter token rate."""

    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, script, colors, **config):
        self.script = script
        self.colors = colors
        super().__init__(text="AI …", **config)

    def poll(self):
        payload = _fetch_payload(self.script)
        if not payload:
            return f'<span foreground="{_color(self.colors, 8)}">AI offline</span>'

        incoming = _compact_count(payload.get("input_tokens_per_minute", 0))
        outgoing = _compact_count(payload.get("output_tokens_per_minute", 0))
        stale = " ~" if payload.get("stale") else ""
        return (
            f'<span foreground="{_color(self.colors, 6)}">{incoming}↓/m</span>\n'
            f'<span foreground="{_color(self.colors, 4)}">{outgoing}↑/m{stale}</span>'
        )


class OpenRouterIOGraph(base._Widget):
    """Five-minute sparkline of sampled input/output tokens per minute."""

    orientations = base.ORIENTATION_HORIZONTAL

    defaults = [
        ("frequency", POLL_SECONDS, "Graph refresh interval in seconds."),
        ("samples", GRAPH_SAMPLES, "Number of rate samples."),
        ("input_color", "#f6019d", "Prompt-token graph color."),
        ("output_color", "#2de2e6", "Completion-token graph color."),
        ("midline_color", "#92406e", "Graph center-line color."),
        ("line_width", 1.4, "Graph line width."),
        ("margin_x", 2, "Horizontal graph margin."),
        ("margin_y", 2, "Vertical graph margin."),
    ]

    def __init__(self, width=GRAPH_WIDTH, **config):
        super().__init__(width, **config)
        self.add_defaults(self.defaults)
        self.input_values = deque(maxlen=self.samples)
        self.output_values = deque(maxlen=self.samples)

    def timer_setup(self):
        self._update()
        self.timeout_add(self.frequency, self.timer_setup)

    def _update(self):
        status = _read_cached_status()
        if status:
            self.input_values.append(float(status.get("input_tokens_per_minute", 0)))
            self.output_values.append(float(status.get("output_tokens_per_minute", 0)))
        self.draw()

    def _draw_series(self, values, color, center_y, half_height, direction):
        if len(values) < 2:
            return

        values = list(values)
        minimum = min(values)
        maximum = max(values)
        if maximum <= minimum:
            return

        # Absolute rates are printed beside the graph. The sparkline uses an
        # adaptive per-stream range so real movement remains visible even when
        # prompt and completion traffic differ by orders of magnitude.
        span = maximum - minimum
        usable_width = max(self.width - 2 * self.margin_x, 1)
        step = usable_width / max(len(values) - 1, 1)

        self.drawer.set_source_rgb(color)
        self.drawer.ctx.set_line_width(self.line_width)

        for index, value in enumerate(values):
            x = self.margin_x + index * step
            normalized = (float(value) - minimum) / span
            distance = (0.15 + 0.85 * normalized) * half_height
            y = center_y + direction * distance
            if index == 0:
                self.drawer.ctx.move_to(x, y)
            else:
                self.drawer.ctx.line_to(x, y)
        self.drawer.ctx.stroke()

    def draw(self):
        self.drawer.clear(self.background or self.bar.background)

        center_y = self.height / 2.0
        half_height = max(center_y - self.margin_y - 1, 1)

        self.drawer.set_source_rgb(self.midline_color)
        self.drawer.ctx.set_line_width(0.6)
        self.drawer.ctx.move_to(self.margin_x, center_y)
        self.drawer.ctx.line_to(self.width - self.margin_x, center_y)
        self.drawer.ctx.stroke()

        self._draw_series(
            self.input_values,
            self.input_color,
            center_y,
            half_height,
            -1,
        )
        self._draw_series(
            self.output_values,
            self.output_color,
            center_y,
            half_height,
            1,
        )
        self.draw_at_default_position()


def _install_sync_and_reload_key(config_globals):
    keys = config_globals.get("keys")
    mod = config_globals.get("mod", "mod4")
    if not isinstance(keys, list):
        return

    modifiers = {mod, "control", "shift"}
    if any(
        getattr(binding, "key", None) == "r"
        and set(getattr(binding, "modifiers", ())) == modifiers
        for binding in keys
    ):
        return

    keys.append(
        Key(
            [mod, "control", "shift"],
            "r",
            lazy.function(_sync_and_reload),
            desc="Sync dotfiles and reload Qtile",
        )
    )


def _telemetry_widgets(home, colors):
    script = home + "/.config/qtile/scripts/openrouter_status.py"
    background = _color(colors, 1)

    return [
        OpenRouterRate(
            script,
            colors,
            name="openrouter_rate",
            update_interval=POLL_SECONDS,
            markup=True,
            font="Hack Nerd Regular",
            fontsize=RATE_FONTSIZE,
            padding=3,
            foreground=_color(colors, 5),
            background=background,
        ),
        OpenRouterIOGraph(
            name="openrouter_io_graph",
            frequency=POLL_SECONDS,
            samples=GRAPH_SAMPLES,
            input_color=_color(colors, 6),
            output_color=_color(colors, 4),
            midline_color=_color(colors, 2),
            background=background,
        ),
    ]


def install_openrouter_widget(config_globals):
    """Install the OpenRouter telemetry cluster and Qtile runtime helpers."""

    _install_sync_and_reload_key(config_globals)

    original_widgets = config_globals.get("widgets")
    separator = config_globals.get("sep")
    home = config_globals.get("home")
    colors = config_globals.get("colors")

    if not callable(original_widgets) or not home or not colors:
        return
    if getattr(original_widgets, "_openrouter_wrapped", False):
        return

    def widgets_with_openrouter():
        items = original_widgets()
        if any(
            str(getattr(item, "name", "")).startswith("openrouter_")
            for item in items
        ):
            return items

        insert_at = next(
            (
                index + 1
                for index, item in enumerate(items)
                if item.__class__.__name__ == "Net"
            ),
            len(items),
        )
        additions = _telemetry_widgets(home, colors)
        if callable(separator):
            additions.insert(0, separator(5))
        items[insert_at:insert_at] = additions
        return items

    widgets_with_openrouter._openrouter_wrapped = True
    config_globals["widgets"] = widgets_with_openrouter
