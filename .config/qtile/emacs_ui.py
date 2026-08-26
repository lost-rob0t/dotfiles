"""Non-blocking, widget-relative Qtile to Emacs popup integration."""

from __future__ import annotations

import json
import logging
import os
import shutil
import subprocess
import threading
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable


DEFAULT_HELPER = Path("~/.config/qtile/qtile-desktop.el").expanduser()
# Popups live in a *named* Emacs server so the user's interactive Emacs is
# never touched: a missing server is started headlessly as a daemon, which by
# definition opens no window (unlike `emacsclient -a emacs`, which spawned a
# second full-size GUI).  Override or disable with QTILE_EMACS_SERVER="".
SERVER_NAME = os.environ.get("QTILE_EMACS_SERVER", "qtile")


@dataclass(frozen=True)
class PopupGeometry:
    """Pixel geometry for a popup on its trigger widget's physical screen."""

    left: int
    top: int
    width: int
    height: int
    screen_x: int
    screen_y: int
    screen_width: int
    screen_height: int


@dataclass(frozen=True)
class PopupLaunch:
    """Result of scheduling a popup launch."""

    started: bool
    reason: str = ""
    geometry: PopupGeometry | None = None


def _number(value: Any, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _screen_rect(screen: Any) -> tuple[int, int, int, int]:
    rect = getattr(screen, "rect", screen)
    return (
        _number(getattr(rect, "x", 0)),
        _number(getattr(rect, "y", 0)),
        max(_number(getattr(rect, "width", 1), 1), 1),
        max(_number(getattr(rect, "height", 1), 1), 1),
    )


def _screen_bars(screen: Any) -> Iterable[Any]:
    seen: set[int] = set()
    for name in ("top", "bottom", "left", "right"):
        bar = getattr(screen, name, None)
        if bar is not None and id(bar) not in seen:
            seen.add(id(bar))
            yield bar
    for bar in getattr(screen, "bars", ()) or ():
        if id(bar) not in seen:
            seen.add(id(bar))
            yield bar


def find_named_widget(qtile: Any, widget_name: str) -> Any | None:
    """Find a named widget across all current physical screens."""
    for screen in getattr(qtile, "screens", ()) or ():
        for bar in _screen_bars(screen):
            for candidate in getattr(bar, "widgets", ()) or ():
                if getattr(candidate, "name", None) == widget_name:
                    return candidate
    return None


def _widget_bar(qtile: Any, widget: Any) -> tuple[Any, Any] | None:
    bar = getattr(widget, "bar", None)
    screen = getattr(bar, "screen", None) if bar is not None else None
    if screen is not None:
        return bar, screen
    for candidate_screen in getattr(qtile, "screens", ()) or ():
        for candidate_bar in _screen_bars(candidate_screen):
            if widget in (getattr(candidate_bar, "widgets", ()) or ()):
                return candidate_bar, candidate_screen
    return None


def _widget_offset(widget: Any) -> int:
    """Horizontal position inside the bar.

    Qtile bar widgets expose ``offset``/``length``; older or stubbed widgets
    may only provide ``x``/``width``.  Reading a nonexistent ``x`` silently
    returned 0 and anchored every popup at the screen's left edge.
    """
    for attribute in ("offset", "offsetx", "x"):
        value = getattr(widget, attribute, None)
        if value is not None:
            return _number(value)
    return 0


def _widget_length(widget: Any) -> int:
    for attribute in ("length", "width"):
        value = getattr(widget, attribute, None)
        if value is not None:
            return max(_number(value, 1), 1)
    return 1


def widget_geometry(qtile: Any, widget_name: str) -> tuple[Any, int, int, int, int] | None:
    """Return ``(screen, x, y, width, height)`` in desktop pixels."""
    widget = find_named_widget(qtile, widget_name)
    if widget is None:
        return None
    owner = _widget_bar(qtile, widget)
    if owner is None:
        return None
    bar, screen = owner
    screen_x, screen_y, _screen_width, _screen_height = _screen_rect(screen)
    width = _widget_length(widget)
    height = max(_number(getattr(widget, "height", 1), 1), 1)
    x = screen_x + _widget_offset(widget)

    position = str(getattr(bar, "position", "top")).casefold()
    bar_height = max(_number(getattr(bar, "height", getattr(bar, "size", height)), height), 1)
    if position == "bottom":
        y = screen_y + _screen_rect(screen)[3] - bar_height
    else:
        y = screen_y
    return screen, x, y, width, height


def popup_geometry(
    qtile: Any,
    widget_name: str,
    *,
    width: int,
    height: int,
    align: str = "left",
) -> PopupGeometry | None:
    """Calculate a clamped popup attached directly below a named widget."""
    if align not in {"left", "center", "right"}:
        raise ValueError(f"unknown popup alignment: {align}")
    details = widget_geometry(qtile, widget_name)
    if details is None:
        return None
    screen, widget_x, bar_top, widget_width, _widget_height = details
    screen_x, screen_y, screen_width, screen_height = _screen_rect(screen)
    popup_width = max(int(width), 1)
    popup_height = max(int(height), 1)
    if align == "center":
        left = widget_x + (widget_width - popup_width) // 2
    elif align == "right":
        left = widget_x + widget_width - popup_width
    else:
        left = widget_x

    left = max(screen_x, min(left, screen_x + screen_width - popup_width))
    # The bar is always the trigger's owning bar; widget_geometry's y is its top.
    bar = getattr(find_named_widget(qtile, widget_name), "bar", None)
    bar_height = max(_number(getattr(bar, "height", getattr(bar, "size", 1)), 1), 1)
    top = bar_top + bar_height
    top = max(screen_y, min(top, screen_y + screen_height - popup_height))
    return PopupGeometry(
        left=left,
        top=top,
        width=popup_width,
        height=popup_height,
        screen_x=screen_x,
        screen_y=screen_y,
        screen_width=screen_width,
        screen_height=screen_height,
    )


def _json_elisp_string(value: Any) -> str:
    return json.dumps(json.dumps(value, ensure_ascii=False), ensure_ascii=False)


def build_emacsclient_command(
    *,
    popup_id: str,
    function: str,
    geometry: PopupGeometry,
    args: dict[str, Any] | None = None,
    helper: Path = DEFAULT_HELPER,
    minibuffer: bool = True,
) -> list[str]:
    """Build a structured, shell-free Emacs server invocation."""
    payload = {
        "geometry": asdict(geometry),
        "args": args or {},
        "minibuffer": bool(minibuffer),
        # A daemon starts without an initial GUI terminal. Pass Qtile's live
        # display so the Elisp side can create the popup on that X display.
        "display": os.environ.get("DISPLAY"),
    }
    expression = (
        "(progn "
        "(add-to-list 'load-path (expand-file-name \"~/.dotfiles/lisp/qtile\")) "
        f"(load-file {json.dumps(str(helper))}) "
        "(require 'qtile-ui) "
        f"(qtile-ui-toggle {json.dumps(popup_id)} {json.dumps(function)} "
        f"(json-read-from-string {_json_elisp_string(payload)})))"
    )
    # Never launch a fallback *editor*.  The named daemon fallback is
    # headless: `emacs --daemon=NAME` opens no window, unlike
    # `emacsclient -a emacs` which spawned a second full-size GUI frame.
    command = ["emacsclient"]
    if SERVER_NAME:
        command += ["-s", SERVER_NAME]
    command += ["-a", "false", "--eval", expression]
    return command


def _report_error(qtile: Any, message: str) -> None:
    logger = getattr(qtile, "log", None)
    if logger is not None and callable(getattr(logger, "error", None)):
        logger.error(message)
    else:
        logging.getLogger(__name__).error(message)


def _notify_user(summary: str, body: str) -> None:
    notifier = shutil.which("notify-send")
    if not notifier:
        return
    try:
        subprocess.Popen(
            [notifier, "-a", "Qtile", summary, body[-300:]],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    except OSError:
        pass


def _is_missing_server(completed: subprocess.CompletedProcess[str]) -> bool:
    detail = (completed.stderr or completed.stdout or "").lower()
    return "socket" in detail or "server" in detail or "no such file" in detail


def _start_named_daemon() -> bool:
    """Start the headless named daemon; it never opens a window."""
    if not SERVER_NAME:
        return False
    emacs = shutil.which("emacs")
    if not emacs:
        return False
    try:
        completed = subprocess.run(
            [emacs, f"--daemon={SERVER_NAME}"],
            check=False,
            capture_output=True,
            text=True,
            timeout=180,
        )
    except (OSError, subprocess.SubprocessError):
        return False
    return completed.returncode == 0


def _try_client(command: list[str]) -> subprocess.CompletedProcess[str]:
    try:
        return subprocess.run(
            command,
            check=False,
            capture_output=True,
            text=True,
            timeout=60,
        )
    except (OSError, subprocess.SubprocessError) as error:
        return subprocess.CompletedProcess(command, 1, "", str(error))


def _run_client(qtile: Any, command: list[str]) -> None:
    result = _try_client(command)
    if result.returncode != 0 and _is_missing_server(result):
        if _start_named_daemon():
            result = _try_client(command)
    if result.returncode != 0:
        detail = (result.stderr or result.stdout or "emacsclient failed").strip()
        _report_error(qtile, f"Qtile Emacs popup failed: {detail[-500:]}")
        if _is_missing_server(result):
            _notify_user(
                "Qtile popup unavailable",
                "Emacs server is not running; start Emacs and try again.",
            )
        else:
            _notify_user("Qtile popup failed", detail)


def toggle_emacs_dropdown(
    qtile: Any,
    *,
    widget_name: str,
    popup_id: str,
    function: str,
    width: int,
    height: int,
    align: str = "left",
    args: dict[str, Any] | None = None,
    helper: Path = DEFAULT_HELPER,
) -> PopupLaunch:
    """Schedule a stable Emacs popup toggle without blocking Qtile."""
    geometry = popup_geometry(
        qtile,
        widget_name,
        width=width,
        height=height,
        align=align,
    )
    if geometry is None:
        return PopupLaunch(False, f"widget not found: {widget_name}")
    command = build_emacsclient_command(
        popup_id=popup_id,
        function=function,
        geometry=geometry,
        args=args,
        helper=helper,
    )
    threading.Thread(
        target=_run_client,
        args=(qtile, command),
        name=f"qtile-emacs-{popup_id}",
        daemon=True,
    ).start()
    return PopupLaunch(True, geometry=geometry)
