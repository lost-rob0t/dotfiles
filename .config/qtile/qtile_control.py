"""Topology-aware Qtile bars, Org integration, and desktop workflows."""

from __future__ import annotations

import ast
import json
import os
import shlex
import subprocess
import threading
from pathlib import Path
from typing import Any, Callable, Iterable

ROLE_ACCENTS = {
    "left": "#f6019d",
    "center": "#2de2e6",
    "right": "#fba922",
    "aux": "#9700cc",
}
DEFAULT_WORKFLOWS = {
    "desktop": {
        "auto_group": True,
        "screens": {"left": "1", "center": "2", "right": "6", "aux": "8"},
    }
}
PRIVATE_ENV_PATH = Path("~/.config/qtile/private.env").expanduser()
WORKFLOWS_PATH = Path("~/.config/qtile/workflows.json").expanduser()
EMACS_HELPER = Path("~/.config/qtile/qtile-desktop.el").expanduser()


def _geometry(item: Any) -> tuple[int, int]:
    rect = getattr(item, "rect", item)
    return int(getattr(rect, "x", 0)), max(int(getattr(rect, "width", 1)), 1)


def screen_roles(items: Iterable[Any]) -> list[str]:
    """Return stable physical roles in the same order as *items*."""
    items = list(items)
    if not items:
        return []

    ordered = sorted(
        [(index, *_geometry(item)) for index, item in enumerate(items)],
        key=lambda row: (row[1], row[2], row[0]),
    )
    roles = ["aux"] * len(items)

    if len(ordered) == 1:
        roles[ordered[0][0]] = "center"
        return roles

    if len(ordered) == 2:
        roles[ordered[0][0]] = "left"
        roles[ordered[1][0]] = "center"
        return roles

    left = ordered[0]
    right = ordered[-1]
    roles[left[0]] = "left"
    roles[right[0]] = "right"

    desktop_left = min(row[1] for row in ordered)
    desktop_right = max(row[1] + row[2] for row in ordered)
    desktop_midpoint = (desktop_left + desktop_right) / 2.0
    middle = ordered[1:-1]
    center = min(
        middle,
        key=lambda row: (abs((row[1] + row[2] / 2.0) - desktop_midpoint), row[1]),
    )
    roles[center[0]] = "center"

    auxiliary = [row for row in middle if row[0] != center[0]]
    for index, row in enumerate(auxiliary, start=1):
        roles[row[0]] = "aux" if len(auxiliary) == 1 else f"aux-{index}"
    return roles


def base_role(role: str) -> str:
    return "aux" if role.startswith("aux") else role


def role_accent(role: str) -> str:
    return ROLE_ACCENTS[base_role(role)]


def parse_private_env(path: Path = PRIVATE_ENV_PATH) -> dict[str, str]:
    values: dict[str, str] = {}
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError:
        return values
    for raw in lines:
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        key = key.strip()
        value = value.strip().strip('"').strip("'")
        if key:
            values[key] = os.path.expanduser(value)
    return values


def load_private_env(path: Path = PRIVATE_ENV_PATH) -> dict[str, str]:
    values = parse_private_env(path)
    for key, value in values.items():
        os.environ.setdefault(key, value)
    return values


def load_workflows(path: Path = WORKFLOWS_PATH) -> dict[str, dict[str, Any]]:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, TypeError):
        return dict(DEFAULT_WORKFLOWS)
    if not isinstance(payload, dict):
        return dict(DEFAULT_WORKFLOWS)
    workflows = {
        str(name): value
        for name, value in payload.items()
        if isinstance(name, str) and isinstance(value, dict)
    }
    return workflows or dict(DEFAULT_WORKFLOWS)


def _decode_emacs_string(output: str) -> str | None:
    output = output.strip()
    if not output or output == "nil":
        return None
    try:
        value = ast.literal_eval(output)
    except (SyntaxError, ValueError):
        return output.strip('"')
    return value if isinstance(value, str) else str(value)


def _emacs_eval(expression: str, *, timeout: float = 300.0) -> str | None:
    helper = str(EMACS_HELPER)
    form = (
        "(progn "
        f"(load-file {json.dumps(helper)}) "
        f"{expression})"
    )
    try:
        completed = subprocess.run(
            ["emacsclient", "-a", "emacs", "--eval", form],
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if completed.returncode != 0:
        return None
    return _decode_emacs_string(completed.stdout)


def _notify(summary: str, body: str = "") -> None:
    command = ["notify-send", "-a", "Qtile", summary]
    if body:
        command.append(body)
    try:
        subprocess.Popen(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except OSError:
        pass


def _role_to_screen_index(screens: Iterable[Any]) -> dict[str, int]:
    screens = list(screens)
    roles = screen_roles(screens)
    result: dict[str, int] = {}
    for index, role in enumerate(roles):
        result[role] = index
        result.setdefault(base_role(role), index)
    return result


def apply_workflow(qtile: Any, workflow: dict[str, Any], config_globals: dict[str, Any]) -> None:
    if workflow.get("auto_group"):
        organizer = config_globals.get("organize_existing_windows")
        if callable(organizer):
            organizer(qtile)

    screens = workflow.get("screens", {})
    if not isinstance(screens, dict):
        return
    role_indices = _role_to_screen_index(qtile.screens)
    used_groups: set[str] = set()
    for role, group_name in screens.items():
        index = role_indices.get(str(role))
        group_name = str(group_name)
        group = qtile.groups_map.get(group_name)
        if index is None or group is None or group_name in used_groups:
            continue
        group.toscreen(index)
        used_groups.add(group_name)

    center = role_indices.get("center")
    if center is not None:
        qtile.focus_screen(center)


def _select_workflow(qtile: Any, config_globals: dict[str, Any]) -> None:
    workflows = load_workflows()
    names = sorted(workflows)
    lisp_names = "(" + " ".join(json.dumps(name) for name in names) + ")"

    def worker() -> None:
        selected = _emacs_eval(f"(qtile-workflow-read '{lisp_names})")
        if not selected or selected not in workflows:
            return
        qtile.call_soon_threadsafe(apply_workflow, qtile, workflows[selected], config_globals)
        _notify("Qtile workflow", selected)

    threading.Thread(target=worker, name="qtile-workflow-picker", daemon=True).start()


def _emacs_frame_command(function: str, title: str) -> str:
    expression = (
        "(progn "
        f"(load-file (expand-file-name {json.dumps(str(EMACS_HELPER))})) "
        f"({function}))"
    )
    frame = f'((name . "{title}") (title . "{title}"))'
    return " ".join(
        [
            "emacsclient",
            "-c",
            "-a",
            "emacs",
            "-F",
            shlex.quote(frame),
            "--eval",
            shlex.quote(expression),
        ]
    )


def _parse_clock_output(output: str) -> str:
    value = _decode_emacs_string(output)
    if not value:
        return "Org: no clock"
    return f"Org: {value}"


def _base_widgets(config_globals: dict[str, Any], accent: str):
    from libqtile import widget

    colors = config_globals["colors"]
    group_names = config_globals["group_names"]
    background = colors[1][0] if isinstance(colors[1], (list, tuple)) else colors[1]
    foreground = colors[5][0] if isinstance(colors[5], (list, tuple)) else colors[5]
    items = []
    auto_group = config_globals.get("auto_group_button")
    if callable(auto_group):
        items.append(auto_group())
    items.extend(
        [
            widget.GroupBox(
                font="3270 Nerd Font",
                visible_groups=group_names,
                fontsize=18,
                margin_y=2,
                margin_x=2,
                padding_y=-4,
                padding_x=6,
                borderwidth=2,
                active=accent,
                inactive=foreground,
                rounded=True,
                highlight_method="block",
                this_current_screen_border=accent,
                this_screen_border=accent,
                other_current_screen_border=background,
                foreground=accent,
                background=background,
            ),
            widget.CurrentLayout(font="Hack Bold", foreground=foreground, background=background),
            widget.WindowName(font="Hack", fontsize=12, foreground=foreground, background=background),
        ]
    )
    return items


def build_screen_widgets(
    role: str,
    config_globals: dict[str, Any],
    telemetry_widgets_factory: Callable[[str, Any], list[Any]],
):
    from libqtile import widget
    from libqtile.lazy import lazy

    colors = config_globals["colors"]
    home = config_globals["home"]
    background = colors[1][0] if isinstance(colors[1], (list, tuple)) else colors[1]
    foreground = colors[5][0] if isinstance(colors[5], (list, tuple)) else colors[5]
    accent = role_accent(role)
    items = _base_widgets(config_globals, accent)
    role = base_role(role)

    if role == "center":
        items.extend(telemetry_widgets_factory(home, colors))
        items.extend(
            [
                widget.Pomodoro(foreground=accent, background=background),
                widget.TextBox(
                    name="agent_zero_button",
                    text=" A0 ",
                    foreground=accent,
                    background=background,
                    mouse_callbacks={
                        "Button1": lazy.group["qtileControl"].dropdown_toggle("agent-zero")
                    },
                ),
                widget.Clock(
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format="%Y-%m-%d %H:%M",
                ),
                widget.Volume(foreground=accent, background=background),
                widget.Systray(background=background, icon_size=20, padding=4),
            ]
        )
    elif role == "left":
        clock_expression = (
            "(if (and (boundp 'org-clock-current-task) org-clock-current-task) "
            "(substring-no-properties org-clock-current-task) nil)"
        )
        items.extend(
            [
                widget.GenPollCommand(
                    name="org_clocked_task",
                    cmd=["emacsclient", "-a", "emacs", "--eval", clock_expression],
                    parse=_parse_clock_output,
                    update_interval=10,
                    foreground=accent,
                    background=background,
                    max_chars=55,
                ),
                widget.TextBox(
                    name="org_todos_button",
                    text=" TODO ",
                    foreground=accent,
                    background=background,
                    mouse_callbacks={
                        "Button1": lazy.group["qtileControl"].dropdown_toggle("org-todos")
                    },
                ),
                widget.TextBox(
                    name="workflow_button",
                    text=" WF ",
                    foreground=accent,
                    background=background,
                    mouse_callbacks={
                        "Button1": lazy.function(_select_workflow, config_globals)
                    },
                ),
                widget.Clock(
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format="%H:%M",
                ),
            ]
        )
    elif role == "right":
        items.extend(
            [
                widget.Mpris2(
                    name="right_mpris",
                    background=background,
                    foreground=accent,
                    scroll_fixed_width=True,
                    poll_interval=1,
                    width=220,
                    padding=10,
                    max_chars=80,
                    markup=False,
                ),
                widget.Clock(
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format="%H:%M",
                ),
                widget.Volume(foreground=accent, background=background),
            ]
        )
    else:
        items.append(
            widget.Clock(
                foreground=foreground,
                background=background,
                fontsize=12,
                format="%H:%M",
            )
        )
    return items


def _install_control_scratchpad(config_globals: dict[str, Any]) -> None:
    from libqtile.config import DropDown, Match, ScratchPad

    groups = config_globals.get("groups")
    if not isinstance(groups, list):
        return
    if any(getattr(group, "name", None) == "qtileControl" for group in groups):
        return
    groups.append(
        ScratchPad(
            "qtileControl",
            [
                DropDown(
                    "org-todos",
                    _emacs_frame_command("qtile-org-todos-open", "qtile-org-todos"),
                    height=0.68,
                    width=0.58,
                    x=0.02,
                    y=0.05,
                    opacity=0.97,
                    on_focus_lost_hide=True,
                    match=Match(title="qtile-org-todos"),
                ),
                DropDown(
                    "agent-zero",
                    _emacs_frame_command("qtile-agent-zero-open", "qtile-agent-zero"),
                    height=0.72,
                    width=0.62,
                    x=0.19,
                    y=0.04,
                    opacity=0.97,
                    on_focus_lost_hide=False,
                    match=Match(title="qtile-agent-zero"),
                ),
            ],
        )
    )


def install_desktop_control(
    config_globals: dict[str, Any],
    telemetry_widgets_factory: Callable[[str, Any], list[Any]],
) -> None:
    """Replace the cloned bar with topology-aware, role-scoped bars."""
    from libqtile import bar
    from libqtile.config import Screen

    load_private_env()
    _install_control_scratchpad(config_globals)

    def screen_widgets(role: str = "center"):
        return build_screen_widgets(role, config_globals, telemetry_widgets_factory)

    def generate_screens(output_info):
        roles = screen_roles(output_info)
        return [
            Screen(
                top=bar.Bar(
                    screen_widgets(role),
                    26,
                    opacity=0.8,
                )
            )
            for role in roles
        ]

    config_globals["screen_widgets"] = screen_widgets
    config_globals["generate_screens"] = generate_screens
