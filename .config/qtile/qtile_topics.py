"""Topic workspaces: a programmatically generated group pool mapped to named topics.

Topics are declarative ``(name, label, layout)`` definitions owned by the
main Qtile Org source.  Each topic generates one prefixed group (``t:<name>``)
so the pool scales without colliding with the static "1".."0" workspaces.
Topics created at runtime through the IPC commands are tracked here so the
bar, auto layouts, and telemetry see the same extended group set.
"""

from __future__ import annotations

import re
import subprocess
import threading
from pathlib import Path
from typing import Any, Callable, Iterable, Sequence

TOPIC_GROUP_PREFIX = "t:"
TOPIC_LAYOUT_DEFAULT = "monadtall"
TOPICS_HELPER = Path("~/.dotfiles/lisp/qtile/qtile-topics.el").expanduser()
TOPICS_WIDGET_ANCHOR = "mara_button"
TOPIC_POPUP_ID = "topics"
TOPIC_PICK_POPUP_ID = "topic-pick"
TOPIC_NAME_CLEANUP = re.compile(r"[^a-z0-9._-]+")

IPC_COMMANDS = (
    ("create_topic", "ipc_create_topic"),
    ("remove_topic", "ipc_remove_topic"),
    ("focus_topic", "ipc_focus_topic"),
    ("send_to_topic", "ipc_send_to_topic"),
)

_default_topics: list[str] = []
_runtime_topics: list[str] = []
_topic_layouts: dict[str, str] = {}


def topic_group_name(name: str) -> str:
    """Return the group name for a topic, tolerating already-prefixed names."""
    name = str(name).strip()
    if name.startswith(TOPIC_GROUP_PREFIX):
        return name
    return f"{TOPIC_GROUP_PREFIX}{name}"


def is_topic_group(name: str) -> bool:
    return str(name).startswith(TOPIC_GROUP_PREFIX)


def normalize_topic_name(name: str) -> str:
    """Reduce a user-supplied topic name to a safe group suffix."""
    cleaned = TOPIC_NAME_CLEANUP.sub("-", str(name).strip().casefold()).strip("-")
    return cleaned


def registered_topic_names() -> list[str]:
    """Default topic groups in declaration order plus runtime-created ones."""
    return [*_default_topics, *_runtime_topics]


def topic_layout(group_name: str) -> str | None:
    return _topic_layouts.get(str(group_name))


def build_topic_groups(definitions: Sequence[Sequence[Any]]) -> list[Any]:
    """Generate one prefixed Group per topic definition and register it.

    Definitions are ``(name[, label[, layout]])``; missing label/layout fall
    back to the topic name and the default layout.  Duplicate topics are
    skipped so repeated config loads stay idempotent.
    """
    from libqtile.config import Group

    groups = []
    for definition in definitions:
        name = str(definition[0])
        label = definition[1] if len(definition) > 1 and definition[1] else name
        layout = definition[2] if len(definition) > 2 and definition[2] else TOPIC_LAYOUT_DEFAULT
        group_name = topic_group_name(name)
        if group_name in _topic_layouts:
            continue
        _default_topics.append(group_name)
        _topic_layouts[group_name] = str(layout)
        groups.append(Group(name=group_name, label=str(label), layout=str(layout)))
    return groups


def _unregister_topic(group_name: str) -> None:
    if group_name in _default_topics:
        _default_topics.remove(group_name)
    if group_name in _runtime_topics:
        _runtime_topics.remove(group_name)
    _topic_layouts.pop(group_name, None)


def topic_groups(qtile: Any) -> list[Any]:
    return [group for group in getattr(qtile, "groups", ()) or () if is_topic_group(group.name)]


def topic_labels(qtile: Any) -> list[list[str]]:
    """Return ``[group name, label]`` pairs for the Emacs dashboard payload."""
    return [
        [group.name, str(getattr(group, "label", "") or group.name)]
        for group in topic_groups(qtile)
    ]


def _telemetry_event(event: str, **fields: Any) -> None:
    try:
        from qtile_telemetry import telemetry_event
    except Exception:
        return
    telemetry_event(event, **fields)


def create_topic(qtile: Any, name: str, label: str | None = None, layout: str | None = None) -> str | None:
    """Register a new topic group on a live Qtile; idempotent per name."""
    from qtile_control import _notify

    cleaned = normalize_topic_name(name)
    if not cleaned:
        _notify("Qtile topic rejected", "Topic needs letters, digits, dot, dash or underscore.")
        return None
    group_name = topic_group_name(cleaned)
    if group_name in qtile.groups_map:
        return group_name
    desired_layout = layout or TOPIC_LAYOUT_DEFAULT
    try:
        created = qtile.add_group(group_name, layout=desired_layout, label=label or cleaned)
    except Exception as error:
        _notify("Qtile topic failed", str(error))
        return None
    if not created:
        return None
    if group_name not in _runtime_topics:
        _runtime_topics.append(group_name)
    _topic_layouts[group_name] = desired_layout
    _telemetry_event("topic_created", topic=group_name)
    _notify("Qtile topic created", group_name)
    return group_name


def remove_topic(qtile: Any, name: str) -> bool:
    """Close a topic group; its windows move to another group, none are killed."""
    from qtile_control import _notify

    group_name = topic_group_name(name)
    if group_name not in qtile.groups_map or group_name not in registered_topic_names():
        _notify("Qtile topic unknown", group_name)
        return False
    try:
        qtile.delete_group(group_name)
    except Exception as error:
        _notify("Qtile topic not removed", str(error))
        return False
    _unregister_topic(group_name)
    _telemetry_event("topic_removed", topic=group_name)
    _notify("Qtile topic closed", group_name)
    return True


def focus_topic(qtile: Any, name: str) -> bool:
    group = qtile.groups_map.get(topic_group_name(name))
    if group is None:
        return False
    group.toscreen()
    return True


def send_current_to_topic(qtile: Any, name: str) -> bool:
    window = getattr(qtile, "current_window", None)
    if window is None:
        return False
    group_name = topic_group_name(name)
    if group_name not in qtile.groups_map:
        return False
    window.togroup(group_name, switch_group=True)
    return True


def step_topic(qtile: Any, step: int) -> bool:
    groups = topic_groups(qtile)
    if not groups:
        return False
    names = [group.name for group in groups]
    current = getattr(getattr(qtile, "current_group", None), "name", None)
    index = names.index(current) if current in names else -step
    return focus_topic(qtile, names[(index + step) % len(names)])


def next_topic(qtile: Any) -> bool:
    return step_topic(qtile, 1)


def previous_topic(qtile: Any) -> bool:
    return step_topic(qtile, -1)


def ipc_create_topic(qtile: Any, name: str) -> str | None:
    return create_topic(qtile, name)


def ipc_remove_topic(qtile: Any, name: str) -> bool:
    return remove_topic(qtile, name)


def ipc_focus_topic(qtile: Any, name: str) -> bool:
    return focus_topic(qtile, name)


def ipc_send_to_topic(qtile: Any, name: str) -> bool:
    return send_current_to_topic(qtile, name)


def expose_topic_commands(root_class: Any | None = None) -> list[str]:
    """Expose topic commands on the Qtile IPC root for `qtile cmd-obj`.

    Example: `qtile cmd-obj -o cmd -f create_topic -a mytopic`.  The root
    class is discovered lazily so importing this module stays testable
    without a live Qtile.
    """
    if root_class is None:
        try:
            from libqtile.core.manager import Qtile as root_class
        except ImportError:
            return []
    commands = getattr(root_class, "_commands", None)
    if commands is None:
        return []
    exposed = []
    for command_name, attribute in IPC_COMMANDS:
        function = globals()[attribute]
        setattr(root_class, command_name, function)
        commands[command_name] = function
        exposed.append(command_name)
    return exposed


def _pick_topic_async(qtile: Any, prompt: str, apply: Callable[[Any, str], Any]) -> None:
    import emacs_ui
    from qtile_control import _decode_emacs_string, _notify

    geometry = emacs_ui.popup_geometry(
        qtile,
        TOPICS_WIDGET_ANCHOR,
        width=440,
        height=380,
        align="left",
    )
    if geometry is None:
        _notify("Qtile topics unavailable", f"widget not found: {TOPICS_WIDGET_ANCHOR}")
        return
    choices = [group.name for group in topic_groups(qtile)]
    if not choices:
        _notify("Qtile topics", "No topic groups exist yet.")
        return
    command = emacs_ui.build_emacsclient_command(
        popup_id=TOPIC_PICK_POPUP_ID,
        function="qtile-topics-pick",
        geometry=geometry,
        args={"choices": choices, "prompt": prompt},
        helper=TOPICS_HELPER,
        minibuffer=True,
    )

    def worker() -> None:
        try:
            completed = subprocess.run(
                command,
                check=False,
                capture_output=True,
                text=True,
                timeout=300,
            )
        except (OSError, subprocess.SubprocessError) as error:
            _notify("Qtile topic picker failed", str(error))
            return
        if completed.returncode != 0:
            _notify("Qtile topic picker failed", (completed.stderr or "emacsclient failed")[-500:])
            return
        selected = _decode_emacs_string(completed.stdout)
        if not selected:
            return
        qtile.call_soon_threadsafe(apply, qtile, selected)

    threading.Thread(target=worker, name="qtile-topic-pick", daemon=True).start()


def _focus_selected(qtile: Any, selected: str) -> None:
    focus_topic(qtile, selected)


def _send_selected(qtile: Any, selected: str) -> None:
    send_current_to_topic(qtile, selected)


def jump_to_topic(qtile: Any) -> None:
    """Mod+g g: pick a topic by name and switch to it."""
    _pick_topic_async(qtile, "Topic: ", _focus_selected)


def move_window_to_topic(qtile: Any) -> None:
    """Mod+Shift+g: pick a topic and move the focused window there."""
    _pick_topic_async(qtile, "Send window to topic: ", _send_selected)


def toggle_topics_dashboard(qtile: Any) -> None:
    """Mod+g t: toggle the Emacs topics dashboard anchored to the center bar."""
    import emacs_ui
    from qtile_control import _notify

    result = emacs_ui.toggle_emacs_dropdown(
        qtile,
        widget_name=TOPICS_WIDGET_ANCHOR,
        popup_id=TOPIC_POPUP_ID,
        function="qtile-topics-open",
        width=560,
        height=480,
        align="left",
        args={"topics": topic_labels(qtile)},
        helper=TOPICS_HELPER,
    )
    if not result.started:
        _notify("Qtile topics unavailable", result.reason)
