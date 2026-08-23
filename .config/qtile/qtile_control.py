"""Topology-aware Qtile bars, truthful group ownership, and desktop workflows."""

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
OUTRUN_EXTENDED = {
    "electric_blue": "#00b8ff",
    "violet": "#6c5ce7",
    "yellow": "#ffe66d",
    "ice": "#7df9ff",
    "hot_orange": "#ff6b35",
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
WORKFLOW_HELPER = Path("~/.config/qtile/qtile-workflow.el").expanduser()
GPT_TODOS_SYNC = Path("~/.dotfiles/scripts/gpt-todos-sync").expanduser()
_group_owner_roles: dict[str, str] = {}
_gpt_todos_sync_lock = threading.Lock()
_gpt_todos_sync_running = False


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


def _color(colors: Any, index: int) -> str:
    value = colors[index]
    if isinstance(value, (list, tuple)):
        return value[0]
    return value


def outrun_palette(colors: Any) -> dict[str, str]:
    """Name the original Doom Outrun palette and a few compatible extensions."""
    return {
        "deep": _color(colors, 0),
        "background": _color(colors, 1),
        "muted": _color(colors, 2),
        "orange": _color(colors, 3),
        "cyan": _color(colors, 4),
        "white": _color(colors, 5),
        "pink": _color(colors, 6),
        "green": _color(colors, 7),
        "red": _color(colors, 8),
        "purple": _color(colors, 9),
        **OUTRUN_EXTENDED,
    }


def next_visible_group(groups: list[Any], current: Any, step: int = 1) -> Any | None:
    """Return the next group after *current* that is visible, or None.

    Navigation is bounded: if no group qualifies (for example an empty
    visible set), the caller gets ``None`` instead of an infinite cycle.
    """
    if not groups:
        return None
    names = [id(group) for group in groups]
    try:
        index = names.index(id(current))
    except ValueError:
        index = None
    if index is None:
        return groups[0] if step > 0 else groups[-1]
    moved = (index + step) % len(groups)
    return groups[moved]


def visible_window_groups(groups: Iterable[Any]) -> list[Any]:
    """Only groups with live windows belong in the bar."""
    return [
        group
        for group in groups
        if getattr(group, "label", None) and bool(getattr(group, "windows", ()))
    ]


def _screen_index(screens: list[Any], screen: Any) -> int | None:
    for index, candidate in enumerate(screens):
        if candidate is screen:
            return index
    return None


def group_owner_role(group: Any, screens: Iterable[Any]) -> str | None:
    """Track the physical screen that most recently owned a non-empty group."""
    name = str(getattr(group, "name", ""))
    if not getattr(group, "windows", ()):
        _group_owner_roles.pop(name, None)
        return None

    screens = list(screens)
    screen = getattr(group, "screen", None)
    if screen is not None:
        index = _screen_index(screens, screen)
        roles = screen_roles(screens)
        if index is not None and index < len(roles):
            role = base_role(roles[index])
            _group_owner_roles[name] = role
            return role
    return _group_owner_roles.get(name)


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
    form = "(progn " f"(load-file {json.dumps(helper)}) " f"{expression})"
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


def _sync_gpt_todos(_qtile: Any) -> None:
    """Run GPT TODO synchronization off the Qtile event loop."""
    global _gpt_todos_sync_running
    with _gpt_todos_sync_lock:
        if _gpt_todos_sync_running:
            _notify("GPT TODO sync", "Already syncing.")
            return
        _gpt_todos_sync_running = True
    _notify("GPT TODO sync", "Synchronizing all agenda files…")

    def worker() -> None:
        global _gpt_todos_sync_running
        try:
            completed = subprocess.run(
                ["bash", str(GPT_TODOS_SYNC)],
                check=False,
                capture_output=True,
                text=True,
                timeout=180,
            )
            detail = (completed.stderr or completed.stdout or "").strip()
            if completed.returncode == 0:
                _notify("GPT TODO sync complete", detail[-500:] or "Agenda files are synchronized.")
            else:
                _notify("GPT TODO sync failed", detail[-800:] or f"exit {completed.returncode}")
        except (OSError, subprocess.SubprocessError) as error:
            _notify("GPT TODO sync failed", str(error))
        finally:
            with _gpt_todos_sync_lock:
                _gpt_todos_sync_running = False

    threading.Thread(target=worker, name="qtile-gpt-todos-sync", daemon=True).start()


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
        helper = json.dumps(str(WORKFLOW_HELPER))
        selected = _emacs_eval(
            f"(progn (load-file {helper}) (qtile-workflow-read-right '{lisp_names}))"
        )
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


def _owned_group_box(config_globals: dict[str, Any]):
    """Return a GroupBox subclass that renders ownership, not cloned bar identity."""
    from libqtile import hook, widget

    colors = config_globals["colors"]
    palette = outrun_palette(colors)
    group_names = config_globals["group_names"]

    class OwnedGroupBox(widget.GroupBox):
        @property
        def groups(self):
            groups = visible_window_groups(self.qtile.groups)
            if self.visible_groups:
                groups = [group for group in groups if group.name in self.visible_groups]
            return groups

        def next_group(self):
            group = next_visible_group(self.groups, self.qtile.current_group)
            if group is not None:
                self.go_to_group(group)

        def prev_group(self):
            group = next_visible_group(self.groups, self.qtile.current_group, step=-1)
            if group is not None:
                self.go_to_group(group)

        def setup_hooks(self):
            super().setup_hooks()
            hook.subscribe.group_window_remove(self._hook_response)

        def remove_hooks(self):
            try:
                hook.unsubscribe.group_window_remove(self._hook_response)
            except Exception:
                pass
            super().remove_hooks()

        def draw(self):
            self.drawer.clear(self.background or self.bar.background)
            offset = self.margin_x
            for group in self.groups:
                owner = group_owner_role(group, self.qtile.screens)
                owner_color = role_accent(owner) if owner else palette["muted"]
                text_color = palette["red"] if self.group_has_urgent(group) else owner_color
                current = self.bar.screen.group == group
                focused = current and self.qtile.current_screen == self.bar.screen
                width = self.box_width([group])
                self.drawbox(
                    offset,
                    group.label,
                    palette["white"] if current else None,
                    text_color,
                    highlight_color=[palette["deep"], owner_color],
                    width=width,
                    rounded=True,
                    block=False,
                    line=current,
                    highlighted=focused,
                )
                offset += width + self.spacing
            self.draw_at_default_position()

    return OwnedGroupBox(
        font="3270 Nerd Font",
        visible_groups=group_names,
        fontsize=18,
        margin_y=2,
        margin_x=2,
        padding_y=-4,
        padding_x=6,
        borderwidth=2,
        active=palette["white"],
        inactive=palette["muted"],
        rounded=True,
        highlight_method="line",
        foreground=palette["white"],
        background=palette["background"],
        urgent_text=palette["red"],
        urgent_border=palette["red"],
    )


def _base_widgets(config_globals: dict[str, Any]):
    from libqtile import widget

    palette = outrun_palette(config_globals["colors"])
    items = []
    auto_group = config_globals.get("auto_group_button")
    if callable(auto_group):
        items.append(auto_group())
    items.extend(
        [
            _owned_group_box(config_globals),
            widget.CurrentLayout(
                font="Hack Bold",
                foreground=palette["white"],
                background=palette["background"],
            ),
            widget.WindowName(
                font="Hack",
                fontsize=12,
                foreground=palette["white"],
                background=palette["background"],
            ),
        ]
    )
    return items


def _system_telemetry(config_globals: dict[str, Any]):
    from libqtile import widget

    palette = outrun_palette(config_globals["colors"])
    background = palette["background"]
    graph_common = {
        "background": background,
        "border_color": palette["muted"],
        "border_width": 1,
        "line_width": 1,
        "frequency": 1,
        "samples": 60,
        "type": "linefill",
        "width": 52,
    }
    return [
        widget.TextBox(
            text="",
            font="Hack Nerd Regular",
            foreground=palette["cyan"],
            background=background,
            padding=2,
        ),
        widget.CPUGraph(
            core="all",
            graph_color=palette["cyan"],
            fill_color=palette["purple"],
            **graph_common,
        ),
        widget.TextBox(
            text="󰍛",
            font="Hack Nerd Regular",
            foreground=palette["green"],
            background=background,
            padding=2,
        ),
        widget.Memory(
            format="{Available: .1f}{mm} free",
            measure_mem="G",
            update_interval=1,
            foreground=palette["green"],
            background=background,
            fontsize=11,
            padding=2,
        ),
        widget.MemoryGraph(
            graph_color=palette["green"],
            fill_color=palette["violet"],
            **graph_common,
        ),
        widget.TextBox(
            text="󰖩",
            font="Hack Nerd Regular",
            foreground=palette["electric_blue"],
            background=background,
            padding=2,
        ),
        widget.NetGraph(
            interface="auto",
            bandwidth_type="down",
            graph_color=palette["electric_blue"],
            fill_color=palette["cyan"],
            **graph_common,
        ),
        widget.NetGraph(
            interface="auto",
            bandwidth_type="up",
            graph_color=palette["pink"],
            fill_color=palette["purple"],
            **graph_common,
        ),
        widget.Net(
            interface="auto",
            format="↓{down}{down_suffix} ↑{up}{up_suffix}",
            update_interval=1,
            foreground=palette["ice"],
            background=background,
            fontsize=11,
            padding=2,
        ),
    ]


def _notification_widget(config_globals: dict[str, Any], role: str):
    from libqtile import widget
    from libqtile.lazy import lazy

    palette = outrun_palette(config_globals["colors"])
    return widget.GenPollCommand(
        name=f"notifications_{role}",
        cmd=["dunstctl", "count", "history"],
        parse=lambda output: f" {output.strip() or '0'}",
        update_interval=1,
        font="Hack Nerd Regular",
        fontsize=11,
        foreground=palette["violet"],
        background=palette["background"],
        padding=3,
        mouse_callbacks={
            "Button1": lazy.spawn("dunstctl history-pop"),
            "Button3": lazy.spawn("dunstctl context"),
            "Button4": lazy.spawn("dunstctl history-pop"),
            "Button5": lazy.spawn("dunstctl close"),
        },
    )


def _weather_widget(config_globals: dict[str, Any]):
    from libqtile import widget

    palette = outrun_palette(config_globals["colors"])
    script = config_globals["home"] + "/.config/qtile/scripts/weather_status.py"
    return widget.GenPollCommand(
        name="outrun_weather",
        cmd=["python3", script],
        update_interval=300,
        foreground=palette["yellow"],
        background=palette["background"],
        font="Hack Nerd Regular",
        fontsize=11,
        padding=4,
    )


def _market_widgets(config_globals: dict[str, Any]):
    from qtile_market import MarketCarousel

    palette = outrun_palette(config_globals["colors"])
    return [
        MarketCarousel(
            name="kalshi_market_stub",
            feed="kalshi",
            foreground=palette["pink"],
            graph_color=palette["pink"],
            accent=palette["cyan"],
            muted=palette["muted"],
            background=palette["background"],
            width=180,
        ),
        MarketCarousel(
            name="commodity_market_stub",
            feed="commodities",
            foreground=palette["orange"],
            graph_color=palette["orange"],
            accent=palette["electric_blue"],
            muted=palette["muted"],
            background=palette["background"],
            width=220,
        ),
    ]


def build_screen_widgets(
    role: str,
    config_globals: dict[str, Any],
    telemetry_widgets_factory: Callable[[str, Any], list[Any]],
    *,
    show_date: bool | None = None,
):
    from libqtile import widget
    from libqtile.lazy import lazy

    palette = outrun_palette(config_globals["colors"])
    home = config_globals["home"]
    background = palette["background"]
    foreground = palette["white"]
    items = _base_widgets(config_globals)
    role = base_role(role)
    if show_date is None:
        # Compatibility fallback for direct screen_widgets() callers. The
        # generated multi-monitor layout overrides this so the left Org screen
        # owns the single date whenever that role exists.
        show_date = role == "center"
    clock_format = "󰃭 %Y-%m-%d   %H:%M" if show_date else " %H:%M"

    if role == "center":
        items.extend(telemetry_widgets_factory(home, config_globals["colors"]))
        items.extend(_system_telemetry(config_globals))
        items.extend(
            [
                widget.Pomodoro(foreground=palette["pink"], background=background),
                widget.TextBox(
                    name="agent_zero_button",
                    text=" A0 ",
                    foreground=palette["cyan"],
                    background=background,
                    mouse_callbacks={
                        "Button1": lazy.group["qtileControl"].dropdown_toggle("agent-zero")
                    },
                ),
                widget.Clock(
                    font="Hack Nerd Regular",
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format=clock_format,
                ),
                widget.Volume(foreground=palette["red"], background=background),
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
                    update_interval=5,
                    foreground=palette["pink"],
                    background=background,
                    max_chars=55,
                ),
                widget.TextBox(
                    name="org_todos_button",
                    text=" TODO ",
                    foreground=palette["cyan"],
                    background=background,
                    mouse_callbacks={
                        "Button1": lazy.group["qtileControl"].dropdown_toggle("org-todos")
                    },
                ),
                widget.TextBox(
                    name="gpt_todos_sync_button",
                    text=" 󰑓 SYNC ",
                    font="Hack Nerd Regular",
                    foreground=palette["green"],
                    background=background,
                    mouse_callbacks={"Button1": lazy.function(_sync_gpt_todos)},
                ),
                widget.TextBox(
                    name="workflow_button",
                    text=" WF ",
                    foreground=palette["orange"],
                    background=background,
                    mouse_callbacks={
                        "Button1": lazy.function(_select_workflow, config_globals)
                    },
                ),
                widget.Clock(
                    font="Hack Nerd Regular",
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format=clock_format,
                ),
                widget.Volume(foreground=palette["red"], background=background),
            ]
        )
    elif role == "right":
        items.extend(_market_widgets(config_globals))
        items.extend(
            [
                _weather_widget(config_globals),
                widget.Mpris2(
                    name="right_mpris",
                    background=background,
                    foreground=palette["cyan"],
                    scroll_fixed_width=True,
                    poll_interval=1,
                    width=220,
                    padding=10,
                    max_chars=80,
                    markup=False,
                ),
                widget.Clock(
                    font="Hack Nerd Regular",
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format=clock_format,
                ),
                widget.Volume(foreground=palette["red"], background=background),
            ]
        )
    else:
        items.extend(
            [
                widget.Clock(
                    font="Hack Nerd Regular",
                    foreground=foreground,
                    background=background,
                    fontsize=12,
                    format=clock_format,
                ),
                widget.Volume(foreground=palette["red"], background=background),
            ]
        )

    items.append(_notification_widget(config_globals, role))
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
                    x=0.42,
                    y=0.02,
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
    """Replace cloned bars with topology-aware, role-scoped bars."""
    from libqtile import bar
    from libqtile.config import Screen

    load_private_env()
    _install_control_scratchpad(config_globals)

    def screen_widgets(role: str = "center", show_date: bool | None = None):
        return build_screen_widgets(
            role,
            config_globals,
            telemetry_widgets_factory,
            show_date=show_date,
        )

    def generate_screens(output_info):
        roles = screen_roles(output_info)
        date_role = "left" if any(base_role(role) == "left" for role in roles) else "center"
        return [
            Screen(
                top=bar.Bar(
                    screen_widgets(role, show_date=base_role(role) == date_role),
                    26,
                    opacity=0.8,
                )
            )
            for role in roles
        ]

    config_globals["screen_widgets"] = screen_widgets
    config_globals["generate_screens"] = generate_screens
