"""Deterministic multi-monitor focus policy for Qtile.

Qtile's X11 backend already focuses the clicked client and that client's
physical screen.  This module keeps that explicit-click path authoritative by
disabling hover-driven focus and cursor warping at config-load time.
"""

from __future__ import annotations

from functools import wraps
from typing import Any, MutableMapping


FOLLOW_MOUSE_FOCUS = False
BRING_FRONT_CLICK = False
CURSOR_WARP = False


def apply_focus_policy(config_globals: MutableMapping[str, Any]) -> None:
    """Apply the stable three-monitor focus policy to Qtile config globals."""
    config_globals["follow_mouse_focus"] = FOLLOW_MOUSE_FOCUS
    config_globals["bring_front_click"] = BRING_FRONT_CLICK
    config_globals["cursor_warp"] = CURSOR_WARP


def install_focus_policy(telemetry_module: Any | None = None) -> None:
    """Install the policy at the config's existing load/reload seam.

    ``config.py`` calls ``qtile_telemetry.install_telemetry(globals())`` on
    every config load, including ``reload_config``. Wrapping that existing
    seam makes the policy survive reloads without adding another Qtile hook
    that would itself be cleared during reload.

    The classifier helpers are also unit-tested without a real Qtile install,
    so an unavailable telemetry runtime is a valid no-op during those imports.
    """
    if telemetry_module is None:
        try:
            import qtile_telemetry as telemetry_module
        except ImportError:
            return

    current = telemetry_module.install_telemetry
    if getattr(current, "_qtile_focus_policy", False):
        return

    @wraps(current)
    def install_with_focus(config_globals: MutableMapping[str, Any]) -> Any:
        apply_focus_policy(config_globals)
        return current(config_globals)

    install_with_focus._qtile_focus_policy = True
    telemetry_module.install_telemetry = install_with_focus


install_focus_policy()
