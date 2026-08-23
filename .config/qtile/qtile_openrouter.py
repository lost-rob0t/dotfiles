"""OpenRouter telemetry widgets and Qtile runtime helpers."""

from __future__ import annotations

import json
import math
import os
import shutil
import subprocess
import threading
import time
from pathlib import Path

from libqtile.config import Key
from libqtile.lazy import lazy
from libqtile.widget import base

import openrouter_history

POLL_SECONDS = 1
GRAPH_SAMPLES = 192
GRAPH_WIDTH = 96
METRIC_FONTSIZE = 12
RATE_FONTSIZE = 10
ROTATE_SECONDS = 5
COLLECTOR_HEARTBEAT_STALE_SECONDS = 30
GRAPH_RANGES = tuple(label for label, _seconds in openrouter_history.TIMEFRAMES)

_poll_lock = threading.Lock()
_last_poll_at = 0.0
_last_payload = None
_sync_lock = threading.Lock()
_sync_in_progress = False
_graph_lock = threading.Lock()
_graph_range_index = 0
_graph_range_changed_at = time.monotonic()


def _color(colors, index):
    value = colors[index]
    if isinstance(value, (list, tuple)):
        return value[0]
    return value


def _cache_path():
    root = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser()
    return root / "qtile" / "openrouter-status.json"


def _read_cached_payload():
    try:
        cache = json.loads(_cache_path().read_text(encoding="utf-8"))
    except (FileNotFoundError, json.JSONDecodeError, OSError, TypeError):
        return None

    status = cache.get("status")
    collector_error = cache.get("collector_error")
    if not isinstance(status, dict):
        if collector_error:
            return {"error": str(collector_error)}
        return {"error": "starting"}

    payload = dict(status)
    try:
        heartbeat = float(cache.get("collector_heartbeat") or 0)
        fetched_at = float(cache.get("fetched_at") or 0)
    except (TypeError, ValueError):
        heartbeat = 0.0
        fetched_at = 0.0
    freshness = heartbeat or fetched_at
    stale = (
        bool(cache.get("collector_stale"))
        or not freshness
        or time.time() - freshness > COLLECTOR_HEARTBEAT_STALE_SECONDS
    )
    payload["stale"] = stale
    payload["collector_pid"] = cache.get("collector_pid")
    payload["collector_parent_pid"] = cache.get("collector_parent_pid")
    payload["collector_heartbeat"] = cache.get("collector_heartbeat")
    payload["collector_error"] = collector_error
    if collector_error and not payload.get("last_error"):
        payload["last_error"] = str(collector_error)
    return payload


def _compact_count(value):
    value = float(value or 0)
    for suffix, divisor in (("B", 1_000_000_000), ("M", 1_000_000), ("k", 1_000)):
        if abs(value) >= divisor:
            rendered = f"{value / divisor:.1f}".rstrip("0").rstrip(".")
            return f"{rendered}{suffix}"
    return str(int(round(value)))


def _fetch_payload(_script):
    """Read local collector state only; widget polling never launches/fetches."""
    global _last_payload, _last_poll_at
    with _poll_lock:
        now = time.monotonic()
        if _last_payload is not None and now - _last_poll_at < POLL_SECONDS - 0.05:
            return _last_payload
        payload = _read_cached_payload()
        _last_poll_at = now
        _last_payload = payload
        return payload


def _start_collector(script):
    """Have Qtile directly ensure the detached provider collector."""
    try:
        return subprocess.Popen(
            [
                "python3",
                script,
                "--daemon",
                "--parent-pid",
                str(os.getpid()),
            ],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
    except OSError:
        return None


def _notify(summary, body="", urgency="normal"):
    notifier = shutil.which("dunstify") or shutil.which("notify-send")
    if not notifier:
        return
    command = [notifier, "-a", "Qtile", "-u", urgency, summary]
    if body:
        command.append(body)
    try:
        subprocess.Popen(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except OSError:
        pass


def _git_sync_command():
    helper = Path.home() / ".config" / "bash" / "git-sync.sh"
    repo = Path.home() / ".dotfiles"
    return f'source "{helper}" && git-sync "{repo}"'


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

    threading.Thread(target=worker, name="qtile-dotfiles-sync", daemon=True).start()


def _rotate_graph_range(now=None):
    global _graph_range_index, _graph_range_changed_at
    with _graph_lock:
        now = time.monotonic() if now is None else float(now)
        elapsed = now - _graph_range_changed_at
        if elapsed >= ROTATE_SECONDS:
            steps = max(int(elapsed // ROTATE_SECONDS), 1)
            _graph_range_index = (_graph_range_index + steps) % len(GRAPH_RANGES)
            _graph_range_changed_at += steps * ROTATE_SECONDS
        return GRAPH_RANGES[_graph_range_index]


def _step_graph_range(step):
    global _graph_range_index, _graph_range_changed_at
    with _graph_lock:
        _graph_range_index = (_graph_range_index + int(step)) % len(GRAPH_RANGES)
        _graph_range_changed_at = time.monotonic()
        return GRAPH_RANGES[_graph_range_index]


def _graph_normalized(value, ceiling):
    """Compress a shared token scale without hiding the smaller I/O series.

    Both input and output use this exact same log1p transform. Zero remains the
    center line, the window ceiling remains full height, and ordering is
    preserved while large prompt-token spikes stop crushing completion-token
    activity into a subpixel line.
    """
    value = max(float(value or 0), 0.0)
    ceiling = max(float(ceiling or 0), 0.0)
    if value <= 0 or ceiling <= 0:
        return 0.0
    return min(math.log1p(value) / math.log1p(ceiling), 1.0)


def _bucket_bounds(sample):
    """Return the provider bucket edges represented by one history sample."""
    center = float(sample.get("timestamp") or 0)
    duration = max(float(sample.get("bucket_seconds") or 1), 1.0)
    return center - duration / 2.0, center + duration / 2.0


class OpenRouterCredit(base.BackgroundPoll):
    """Display current OpenRouter account balance without blocking Qtile."""

    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, script, colors, **config):
        self.script = script
        self.colors = colors
        super().__init__(text="$…", **config)

    def poll(self):
        payload = _fetch_payload(self.script)
        if not payload or payload.get("balance_usd") is None:
            return f'<span foreground="{_color(self.colors, 8)}">$?</span>'
        balance = float(payload["balance_usd"])
        color = (
            _color(self.colors, 8)
            if balance < 5
            else _color(self.colors, 3)
            if balance < 10
            else _color(self.colors, 7)
        )
        stale = " ~" if payload.get("stale") else ""
        return f'<span foreground="{color}">${balance:.2f}{stale}</span>'


class OpenRouterRate(base.BackgroundPoll):
    """Display the most recent trusted OpenRouter token rate."""

    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, script, colors, **config):
        self.script = script
        self.colors = colors
        super().__init__(text="AI …", **config)

    def poll(self):
        payload = _fetch_payload(self.script)
        if not payload or payload.get("error"):
            reason = (payload or {}).get("error", "offline")
            return f'<span foreground="{_color(self.colors, 8)}">AI {reason}</span>'
        incoming = _compact_count(payload.get("input_tokens_per_minute", 0))
        outgoing = _compact_count(payload.get("output_tokens_per_minute", 0))
        warning = " !" if payload.get("last_error") else ""
        stale = " ~" if payload.get("stale") else ""
        return (
            f'<span foreground="{_color(self.colors, 6)}">{incoming}↓/m</span>\n'
            f'<span foreground="{_color(self.colors, 4)}">{outgoing}↑/m{warning}{stale}</span>'
        )


class OpenRouterRotatingMetric(base.BackgroundPoll):
    """Rotate one OpenRouter metric through configured periods."""

    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, script, colors, metric, **config):
        self.script = script
        self.colors = colors
        self.metric = metric
        self.index = 0
        self.last_rotate = time.monotonic()
        self.specs = (
            (("M", "tokens_month"), ("W", "tokens_week"), ("D", "tokens_day"), ("H", "tokens_hour"))
            if metric == "tokens"
            else (("D", "spend_day"), ("W", "spend_week"), ("M", "spend_month"))
        )
        self.icon = "" if metric == "tokens" else ""
        super().__init__(text=f"{self.icon} …", **config)
        self.add_callbacks({"Button4": self.previous, "Button5": self.next})

    def previous(self):
        self.index = (self.index - 1) % len(self.specs)
        self.last_rotate = time.monotonic()
        self.tick()

    def next(self):
        self.index = (self.index + 1) % len(self.specs)
        self.last_rotate = time.monotonic()
        self.tick()

    def _maybe_rotate(self):
        now = time.monotonic()
        if now - self.last_rotate >= ROTATE_SECONDS:
            self.index = (self.index + 1) % len(self.specs)
            self.last_rotate = now

    def poll(self):
        self._maybe_rotate()
        label, key = self.specs[self.index]
        payload = _fetch_payload(self.script) or {}
        value = payload.get(key)
        color_index = (4, 6, 3, 7)[self.index % 4]
        color = _color(self.colors, color_index)
        if value is None:
            rendered = "!" if payload.get("usage_error") else "—"
        elif self.metric == "tokens":
            rendered = _compact_count(value)
        else:
            rendered = f"${float(value):.2f}"
        return f'<span foreground="{color}">{self.icon} {label} {rendered}</span>'


class OpenRouterGraphRange(base.BackgroundPoll):
    """Visible controller for the persistent graph range."""

    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, colors, **config):
        self.colors = colors
        super().__init__(text="󰓅 1m", **config)
        self.add_callbacks({"Button1": self.next, "Button4": self.previous, "Button5": self.next})

    def previous(self):
        _step_graph_range(-1)
        self.tick()

    def next(self):
        _step_graph_range(1)
        self.tick()

    def poll(self):
        label = _rotate_graph_range()
        return f'<span foreground="{_color(self.colors, 3)}">󰓅 {label}</span>'


class OpenRouterIOGraph(base._Widget):
    """Persistent I/O graph with shared log scaling and truthful bucket edges."""

    orientations = base.ORIENTATION_HORIZONTAL
    defaults = [
        ("frequency", POLL_SECONDS, "Graph refresh interval in seconds."),
        ("samples", GRAPH_SAMPLES, "Maximum local history points."),
        ("input_color", "#f6019d", "Prompt-token graph color."),
        ("output_color", "#2de2e6", "Completion-token graph color."),
        ("midline_color", "#92406e", "Graph center-line color."),
        ("line_width", 1.8, "Graph line width."),
        ("margin_x", 2, "Horizontal graph margin."),
        ("margin_y", 2, "Vertical graph margin."),
    ]

    def __init__(self, width=GRAPH_WIDTH, **config):
        super().__init__(width, **config)
        self.add_defaults(self.defaults)
        self.series = {"range": "1m", "start": 0, "end": 1, "ceiling": 0, "samples": []}
        self._last_signature = None
        self._query_lock = threading.Lock()
        self._query_running = False

    def timer_setup(self):
        self._update()
        self.timeout_add(self.frequency, self.timer_setup)

    def _update(self):
        label = _rotate_graph_range()
        with self._query_lock:
            if self._query_running:
                return
            self._query_running = True

        def worker():
            try:
                series = openrouter_history.query_series(label, points=self.samples)
            except Exception:
                series = None

            def apply():
                with self._query_lock:
                    self._query_running = False
                if series is None:
                    return
                samples = series.get("samples", [])
                signature = (
                    label,
                    tuple(
                        (
                            sample.get("timestamp"),
                            sample.get("input"),
                            sample.get("output"),
                            sample.get("bucket_seconds"),
                        )
                        for sample in samples
                    ),
                )
                if signature != self._last_signature:
                    self.series = series
                    self._last_signature = signature
                    self.draw()

            try:
                self.qtile.call_soon_threadsafe(apply)
            except Exception:
                with self._query_lock:
                    self._query_running = False

        threading.Thread(
            target=worker,
            name="qtile-openrouter-graph",
            daemon=True,
        ).start()

    def _draw_series(self, key, color, center_y, half_height, direction, ceiling):
        """Draw provider buckets as steps; never interpolate ramps across gaps."""
        samples = self.series.get("samples", [])
        if not samples or ceiling <= 0:
            return
        start = float(self.series.get("start", 0))
        end = float(self.series.get("end", start + 1))
        span = max(end - start, 1.0)
        usable_width = max(self.width - 2 * self.margin_x, 1)
        self.drawer.set_source_rgb(color)
        self.drawer.ctx.set_line_width(self.line_width)
        previous_end = None
        previous_y = None
        for sample in samples:
            bucket_start, bucket_end = _bucket_bounds(sample)
            clipped_start = max(bucket_start, start)
            clipped_end = min(bucket_end, end)
            if clipped_end <= clipped_start:
                continue
            x0 = self.margin_x + ((clipped_start - start) / span) * usable_width
            x1 = self.margin_x + ((clipped_end - start) / span) * usable_width
            normalized = _graph_normalized(sample.get(key, 0), ceiling)
            y = center_y + direction * normalized * half_height
            contiguous = previous_end is not None and abs(clipped_start - previous_end) <= 0.5
            if contiguous and previous_y is not None:
                self.drawer.ctx.move_to(x0, previous_y)
                self.drawer.ctx.line_to(x0, y)
            else:
                self.drawer.ctx.move_to(x0, y)
            self.drawer.ctx.line_to(x1, y)
            previous_end = clipped_end
            previous_y = y
        self.drawer.ctx.stroke()

    def draw(self):
        self.drawer.clear(self.background or self.bar.background)
        center_y = self.height / 2.0
        half_height = max(center_y - self.margin_y - 1, 1)
        self.drawer.set_source_rgb(self.midline_color)
        self.drawer.ctx.set_line_width(0.5)
        self.drawer.ctx.move_to(self.margin_x, center_y)
        self.drawer.ctx.line_to(self.width - self.margin_x, center_y)
        self.drawer.ctx.stroke()
        ceiling = float(self.series.get("ceiling") or 0)
        self._draw_series("input", self.input_color, center_y, half_height, -1, ceiling)
        self._draw_series("output", self.output_color, center_y, half_height, 1, ceiling)
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
    common = {
        "update_interval": POLL_SECONDS,
        "markup": True,
        "font": "Hack Nerd Regular",
        "fontsize": METRIC_FONTSIZE,
        "padding": 3,
        "foreground": _color(colors, 5),
        "background": background,
    }
    rate_common = {
        **common,
        "fontsize": RATE_FONTSIZE,
        "padding": 2,
    }
    return [
        OpenRouterCredit(script, colors, name="openrouter_credit", **common),
        OpenRouterRate(script, colors, name="openrouter_rate", **rate_common),
        OpenRouterGraphRange(colors, name="openrouter_graph_range", **common),
        OpenRouterIOGraph(
            name="openrouter_io_graph",
            frequency=POLL_SECONDS,
            samples=GRAPH_SAMPLES,
            input_color=_color(colors, 6),
            output_color=_color(colors, 4),
            midline_color=_color(colors, 2),
            background=background,
        ),
        OpenRouterRotatingMetric(script, colors, "tokens", name="openrouter_token_totals", **common),
        OpenRouterRotatingMetric(script, colors, "spend", name="openrouter_spend_totals", **common),
    ]


def install_openrouter_widget(config_globals):
    """Install OpenRouter telemetry and the topology-aware desktop control layer."""
    _install_sync_and_reload_key(config_globals)
    from qtile_control import install_desktop_control

    install_desktop_control(config_globals, _telemetry_widgets)
    script = config_globals["home"] + "/.config/qtile/scripts/openrouter_status.py"
    _start_collector(script)
