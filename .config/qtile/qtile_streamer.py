"""Streamer privacy mode: allow-list enforcement for the center screen.

Streamer mode is session-only and starts disabled on every Qtile start.
While armed, only allow-listed windows (Nyxt, the sandboxed StreamEmacs,
OpenCode terminals — see ``STREAM_ALLOWLIST`` in qtile_ai_windows.py) may
occupy the group shown on the center screen.  Any other window landing
there is moved to the holding group on another screen instead of being
killed, and enabling sweeps existing violations off the center screen.
"""

from __future__ import annotations

import subprocess
from typing import Any

from qtile_ai_windows import is_stream_allowed
from qtile_control import _notify, outrun_palette

STREAM_CAPTURE_SCRIPT = "screen-capture"
RECORD_SUBCOMMAND = "record-screen"
STOP_SUBCOMMAND = "stop"
HOLDING_GROUP = "hold"
STREAM_BUTTON_TEXT = " STREAM "
STREAM_BUTTON_FONT = "Hack Nerd Regular"

_state: dict[str, bool] = {"enabled": False, "recording": False}
_buttons: list[Any] = []
_button_background = "#92406e"
_button_armed = "#f6019d"
_BUTTON_TEXT_COLOR = "#000000"


def is_enabled() -> bool:
    return _state["enabled"]


def is_recording() -> bool:
    return _state["recording"]


def center_screen_index(qtile: Any) -> int | None:
    from qtile_control import screen_roles

    screens = list(getattr(qtile, "screens", ()) or ())
    if not screens:
        return None
    roles = screen_roles(screens)
    return roles.index("center") if "center" in roles else None


def window_is_on_center(qtile: Any, group: Any) -> bool:
    index = center_screen_index(qtile)
    if index is None:
        return False
    screens = list(qtile.screens)
    if index >= len(screens):
        return False
    screen = getattr(group, "screen", None)
    return screen is not None and screen is screens[index]


def window_wm_classes(window: Any) -> tuple[str, ...]:
    try:
        return tuple(window.window.get_wm_class() or ())
    except (AttributeError, TypeError):
        return ()


def window_title(window: Any) -> str:
    return str(getattr(window, "name", "") or "")


def is_privacy_violation(qtile: Any, window: Any) -> bool:
    """A violation is a non-allow-listed window whose group owns the center screen."""
    if is_stream_allowed(window_wm_classes(window), window_title(window)):
        return False
    group = getattr(window, "group", None)
    if group is None:
        return False
    return window_is_on_center(qtile, group)


def ensure_holding_group(qtile: Any) -> None:
    if HOLDING_GROUP in qtile.groups_map:
        return
    try:
        qtile.add_group(HOLDING_GROUP, label=HOLDING_GROUP, persist=True)
    except Exception:
        pass


def move_to_holding(qtile: Any, window: Any) -> bool:
    ensure_holding_group(qtile)
    if HOLDING_GROUP not in qtile.groups_map:
        return False
    source = getattr(getattr(window, "group", None), "name", None)
    try:
        window.togroup(HOLDING_GROUP, switch_group=False)
    except Exception:
        return False
    _telemetry_auto_route(window, source, HOLDING_GROUP)
    return True


def enforce_streamer(qtile: Any, window: Any) -> bool:
    if not _state["enabled"]:
        return False
    if not is_privacy_violation(qtile, window):
        return False
    return move_to_holding(qtile, window)


def sweep_center(qtile: Any) -> int:
    """Move every existing violation off the center screen; return the count."""
    moved = 0
    for group in list(getattr(qtile, "groups", ()) or ()):
        for window in tuple(getattr(group, "windows", ()) or ()):
            if is_privacy_violation(qtile, window) and move_to_holding(qtile, window):
                moved += 1
    return moved


def _telemetry_event(event: str, **fields: Any) -> None:
    try:
        from qtile_telemetry import telemetry_event
    except Exception:
        return
    telemetry_event(event, **fields)


def _telemetry_auto_route(window: Any, source: Any, target: str) -> None:
    try:
        from qtile_telemetry import telemetry_auto_route
    except Exception:
        return
    telemetry_auto_route(window, source, target)


def _spawn_capture(*arguments: str) -> bool:
    try:
        subprocess.Popen(
            [STREAM_CAPTURE_SCRIPT, *arguments],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
        return True
    except OSError as error:
        _notify("Streamer capture failed", str(error))
        return False


def start_recording() -> bool:
    if _state["recording"]:
        return False
    if not _spawn_capture(RECORD_SUBCOMMAND):
        return False
    _state["recording"] = True
    _telemetry_event("streamer_recording_changed", recording=True)
    return True


def stop_recording() -> bool:
    if not _state["recording"]:
        return False
    _state["recording"] = False
    spawned = _spawn_capture(STOP_SUBCOMMAND)
    _telemetry_event("streamer_recording_changed", recording=False)
    return spawned


def set_streamer_mode(qtile: Any, enabled: bool) -> bool:
    enabled = bool(enabled)
    if _state["enabled"] == enabled:
        return False
    _state["enabled"] = enabled
    update_streamer_buttons()
    _telemetry_event("streamer_mode_changed", enabled=enabled, source="keybind")
    if enabled:
        swept = sweep_center(qtile)
        detail = f"Center allow-listed; {swept} window(s) moved to hold."
        _notify("Streamer mode ON", detail)
    else:
        if _state["recording"]:
            stop_recording()
        _notify("Streamer mode OFF", "Center screen is unrestricted.")
    return True


def toggle_streamer_mode(qtile: Any, record: bool = False) -> None:
    enable = not _state["enabled"]
    set_streamer_mode(qtile, enable)
    if record:
        if enable:
            start_recording()
        elif _state["recording"]:
            stop_recording()


def _button_colors() -> tuple[str, str]:
    return _BUTTON_TEXT_COLOR, (_button_armed if _state["enabled"] else _button_background)


def update_streamer_buttons() -> None:
    foreground, background = _button_colors()
    for button in list(_buttons):
        try:
            button.foreground = foreground
            button.background = background
            button.draw()
        except Exception:
            continue


def streamer_button(config_globals: dict[str, Any]):
    """Create the bar toggle button mirroring the AUTO button pattern."""
    from libqtile import widget
    from libqtile.lazy import lazy

    global _button_background, _button_armed
    palette = outrun_palette(config_globals["colors"])
    _button_background = palette["muted"]
    _button_armed = palette["pink"]
    foreground, background = _button_colors()
    button = widget.TextBox(
        name=f"streamer_mode_{len(_buttons)}",
        text=STREAM_BUTTON_TEXT,
        font=STREAM_BUTTON_FONT,
        fontsize=12,
        padding=8,
        foreground=foreground,
        background=background,
        mouse_callbacks={"Button1": lazy.function(toggle_streamer_mode)},
    )
    _buttons.append(button)
    return button
