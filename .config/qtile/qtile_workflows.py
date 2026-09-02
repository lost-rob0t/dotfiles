"""Declarative Qtile workflow actions layered over qtile_control."""

from __future__ import annotations

import os
import subprocess
import threading
from collections.abc import Iterable, Mapping
from typing import Any

import qtile_control


_WORKFLOW_WRAPPER_ATTR = "_qtile_workflow_runtime_wrapper"
_LAYOUT_WRAPPER_ATTR = "_qtile_workflow_layout_wrapper"
_LAYOUT_OVERRIDES_ATTR = "_qtile_workflow_layout_overrides"
_BASE_APPLY_WORKFLOW = qtile_control.apply_workflow


def _notify(summary: str, body: str = "") -> None:
    notifier = getattr(qtile_control, "_notify", None)
    if callable(notifier):
        notifier(summary, body)


def _close_all_client_windows(qtile: Any) -> None:
    """Kill every client window owned by a Qtile group, including scratchpads."""
    seen: set[int] = set()
    for group in tuple(getattr(qtile, "groups", ()) or ()):
        for window in tuple(getattr(group, "windows", ()) or ()):
            marker = id(window)
            if marker in seen:
                continue
            seen.add(marker)
            try:
                window.kill()
            except Exception:
                continue


def _set_auto_mode(config_globals: dict[str, Any], enabled: bool) -> None:
    """Set the real Auto-mode global and refresh its widgets/telemetry."""
    previous = bool(config_globals.get("auto_group_mode", False))
    config_globals["auto_group_mode"] = bool(enabled)

    updater = config_globals.get("update_auto_group_buttons")
    if callable(updater):
        updater()

    if previous == bool(enabled):
        return
    try:
        from qtile_telemetry import telemetry_event

        telemetry_event("auto_mode_changed", enabled=bool(enabled), source="workflow")
    except (ImportError, AttributeError):
        pass


def _layout_wrapper(config_globals: dict[str, Any]):
    updater = config_globals.get("update_group_layout")
    if not callable(updater):
        return None
    if getattr(updater, _LAYOUT_WRAPPER_ATTR, False):
        return updater

    original = updater

    def wrapped(group: Any) -> Any:
        overrides = getattr(wrapped, _LAYOUT_OVERRIDES_ATTR)
        desired = overrides.get(str(getattr(group, "name", "")))
        if desired:
            layout_name = getattr(getattr(group, "layout", None), "name", None)
            if layout_name != desired:
                group.setlayout(desired)
            return None
        return original(group)

    setattr(wrapped, _LAYOUT_WRAPPER_ATTR, True)
    setattr(wrapped, _LAYOUT_OVERRIDES_ATTR, {})
    config_globals["update_group_layout"] = wrapped
    return wrapped


def _set_layout_overrides(
    qtile: Any,
    config_globals: dict[str, Any],
    overrides: Mapping[str, Any] | None,
) -> None:
    wrapper = _layout_wrapper(config_globals)
    if wrapper is None:
        return

    state = getattr(wrapper, _LAYOUT_OVERRIDES_ATTR)
    state.clear()
    for group_name, layout_name in (overrides or {}).items():
        group_name = str(group_name).strip()
        layout_name = str(layout_name).strip().lower()
        if group_name and layout_name:
            state[group_name] = layout_name

    updater = config_globals.get("update_auto_layouts")
    if callable(updater):
        updater(qtile)


def _expand_token(value: Any) -> str:
    return os.path.expanduser(os.path.expandvars(str(value)))


def _launch_one(spec: Mapping[str, Any]) -> None:
    argv = spec.get("argv")
    if not isinstance(argv, Iterable) or isinstance(argv, (str, bytes)):
        raise ValueError("workflow launch entry requires an argv list")

    command = [_expand_token(value) for value in argv]
    if not command:
        raise ValueError("workflow launch argv cannot be empty")

    cwd_value = spec.get("cwd")
    cwd = _expand_token(cwd_value) if cwd_value else None
    subprocess.Popen(
        command,
        cwd=cwd,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )


def _launch_all(entries: Iterable[Any]) -> None:
    for entry in entries:
        if not isinstance(entry, Mapping):
            continue
        try:
            _launch_one(entry)
        except (OSError, ValueError) as error:
            _notify("Qtile workflow launch failed", str(error))


def _launch_async(entries: Any) -> None:
    if not isinstance(entries, Iterable) or isinstance(entries, (str, bytes, Mapping)):
        return
    entries = tuple(entries)
    if not entries:
        return
    threading.Thread(
        target=_launch_all,
        args=(entries,),
        name="qtile-workflow-launch",
        daemon=True,
    ).start()


def apply_workflow(
    qtile: Any,
    workflow: dict[str, Any],
    config_globals: dict[str, Any],
) -> None:
    """Apply base screen placement plus declarative lifecycle/actions."""
    if workflow.get("close_all"):
        _close_all_client_windows(qtile)

    auto_mode = workflow.get("auto_mode")
    if isinstance(auto_mode, bool):
        _set_auto_mode(config_globals, auto_mode)

    _set_layout_overrides(qtile, config_globals, workflow.get("layouts"))
    _BASE_APPLY_WORKFLOW(qtile, workflow, config_globals)
    _set_layout_overrides(qtile, config_globals, workflow.get("layouts"))
    _launch_async(workflow.get("launch"))


setattr(apply_workflow, _WORKFLOW_WRAPPER_ATTR, True)
setattr(apply_workflow, "_qtile_workflow_base", _BASE_APPLY_WORKFLOW)

if not getattr(qtile_control.apply_workflow, _WORKFLOW_WRAPPER_ATTR, False):
    qtile_control.apply_workflow = apply_workflow
