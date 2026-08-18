import json
import os
from datetime import datetime, timezone
from pathlib import Path

from libqtile import hook, qtile
from libqtile.lazy import lazy

_config = None
_state_dir = Path(
    os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state")
) / "qtile"
_log_path = _state_dir / "telemetry.jsonl"
_session = f"{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')}-{os.getpid()}"
_max_bytes = 25 * 1024 * 1024
_backups = 4


def _auto_mode():
    return bool(_config and _config.get("auto_group_mode", False))


def _rotate():
    if not _log_path.exists() or _log_path.stat().st_size < _max_bytes:
        return
    oldest = _log_path.with_suffix(f"{_log_path.suffix}.{_backups}")
    if oldest.exists():
        oldest.unlink()
    for index in range(_backups - 1, 0, -1):
        source = _log_path.with_suffix(f"{_log_path.suffix}.{index}")
        if source.exists():
            source.replace(_log_path.with_suffix(f"{_log_path.suffix}.{index + 1}"))
    _log_path.replace(_log_path.with_suffix(f"{_log_path.suffix}.1"))


def telemetry_event(event, **fields):
    record = {
        "schema_version": 1,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "session": _session,
        "event": event,
        "auto_mode": _auto_mode(),
        **fields,
    }
    try:
        _state_dir.mkdir(parents=True, exist_ok=True)
        _rotate()
        with _log_path.open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record, ensure_ascii=False, sort_keys=True) + "\n")
    except OSError:
        pass


def telemetry_auto_route(window, source_group, target_group):
    telemetry_event(
        "window_auto_routed",
        source_group=source_group,
        target_group=target_group,
        window=_window(window),
    )


def _window(window):
    if window is None:
        return None
    try:
        wm_class = list(window.window.get_wm_class() or ())
    except (AttributeError, TypeError):
        wm_class = []
    try:
        pid = window.window.get_net_wm_pid()
    except AttributeError:
        pid = None
    group = getattr(window, "group", None)
    screen = getattr(group, "screen", None)
    return {
        "window_id": getattr(window, "wid", None),
        "pid": pid,
        "title": str(getattr(window, "name", "") or "")[:300],
        "wm_class": wm_class,
        "group": getattr(group, "name", None),
        "layout": getattr(getattr(group, "layout", None), "name", None),
        "screen": getattr(screen, "index", None),
        "floating": bool(getattr(window, "floating", False)),
        "fullscreen": bool(getattr(window, "fullscreen", False)),
        "minimized": bool(getattr(window, "minimized", False)),
        "geometry": {
            "x": getattr(window, "x", None),
            "y": getattr(window, "y", None),
            "width": getattr(window, "width", None),
            "height": getattr(window, "height", None),
        },
    }


def _binding(modifiers, key):
    return "+".join([*modifiers, str(key)])


def _keymap(mappings, chord=()):
    entries = []
    for mapping in mappings:
        binding = _binding(
            getattr(mapping, "modifiers", ()),
            getattr(mapping, "key", ""),
        )
        path = [*chord, binding]
        entries.append(
            {
                "binding": binding,
                "path": path,
                "description": getattr(mapping, "desc", "") or None,
                "type": mapping.__class__.__name__,
            }
        )
        submappings = getattr(mapping, "submappings", None)
        if submappings:
            entries.extend(_keymap(submappings, tuple(path)))
    return entries


def _log_keybind(qtile_instance, binding, path, description):
    group = getattr(qtile_instance, "current_group", None)
    telemetry_event(
        "keybind",
        binding=binding,
        path=path,
        description=description or None,
        group=getattr(group, "name", None),
        layout=getattr(getattr(group, "layout", None), "name", None),
        window=_window(getattr(qtile_instance, "current_window", None)),
    )


def _instrument_keys(mappings, chord=()):
    for mapping in mappings:
        binding = _binding(
            getattr(mapping, "modifiers", ()),
            getattr(mapping, "key", ""),
        )
        path = [*chord, binding]
        commands = getattr(mapping, "commands", None)
        if commands is not None and not getattr(mapping, "_telemetry_instrumented", False):
            logger = lazy.function(
                _log_keybind,
                binding,
                path,
                getattr(mapping, "desc", ""),
            )
            mapping.commands = [logger, *commands]
            mapping._telemetry_instrumented = True
        submappings = getattr(mapping, "submappings", None)
        if submappings:
            _instrument_keys(submappings, tuple(path))


def install_telemetry(config_globals):
    global _config
    _config = config_globals
    _instrument_keys(config_globals.get("keys", ()))

    from qtile_openrouter import install_openrouter_widget

    install_openrouter_widget(config_globals)


@hook.subscribe.startup_complete
def _startup_complete():
    if not _config:
        return
    group_names = _config.get("group_names", ())
    groups = []
    for name in group_names:
        group = qtile.groups_map.get(name)
        if group is None:
            continue
        groups.append(
            {
                "group": name,
                "layout": getattr(getattr(group, "layout", None), "name", None),
                "window_count": len(getattr(group, "windows", ())),
                "windows": [_window(window) for window in getattr(group, "windows", ())],
            }
        )
    telemetry_event(
        "session_start",
        log_path=str(_log_path),
        groups=groups,
        keymap=_keymap(_config.get("keys", ())),
    )


@hook.subscribe.client_managed
def _client_managed(window):
    route = None
    if _config and _config.get("routed_group"):
        route = _config["routed_group"](window)
    telemetry_event("window_managed", route_target=route, window=_window(window))


@hook.subscribe.group_window_add
def _group_window_add(group, window):
    telemetry_event(
        "window_group_add",
        group=group.name,
        layout=getattr(group.layout, "name", None),
        window_count=len(group.windows),
        window=_window(window),
    )


@hook.subscribe.group_window_remove
def _group_window_remove(group, window):
    telemetry_event(
        "window_group_remove",
        group=group.name,
        layout=getattr(group.layout, "name", None),
        window_count=len(group.windows),
        window=_window(window),
    )


@hook.subscribe.layout_change
def _layout_change(current_layout, group):
    telemetry_event(
        "layout_change",
        group=group.name,
        layout=current_layout.name,
        window_count=len(group.windows),
    )


@hook.subscribe.client_focus
def _client_focus(window):
    telemetry_event("window_focus", window=_window(window))


@hook.subscribe.float_change
def _float_change():
    telemetry_event("float_change", window=_window(qtile.current_window))


@hook.subscribe.setgroup
def _setgroup():
    screen = getattr(qtile, "current_screen", None)
    group = getattr(screen, "group", None)
    telemetry_event(
        "group_set",
        screen=getattr(screen, "index", None),
        group=getattr(group, "name", None),
        layout=getattr(getattr(group, "layout", None), "name", None),
        window_count=len(getattr(group, "windows", ())),
    )


@hook.subscribe.current_screen_change
def _screen_change():
    screen = getattr(qtile, "current_screen", None)
    telemetry_event(
        "screen_change",
        screen=getattr(screen, "index", None),
        group=getattr(getattr(screen, "group", None), "name", None),
    )


@hook.subscribe.enter_chord
def _enter_chord(name):
    telemetry_event("key_chord_enter", name=name)


@hook.subscribe.leave_chord
def _leave_chord():
    telemetry_event("key_chord_leave")


@hook.subscribe.client_killed
def _client_killed(window):
    telemetry_event("window_killed", window=_window(window))


@hook.subscribe.restart
def _restart():
    telemetry_event("session_restart")


@hook.subscribe.shutdown
def _shutdown():
    telemetry_event("session_shutdown")
