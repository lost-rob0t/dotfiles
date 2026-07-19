#!/usr/bin/env python3
import argparse
import json
import os
from collections import Counter
from pathlib import Path


def default_log_path():
    state_home = Path(
        os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state")
    )
    return state_home / "qtile" / "telemetry.jsonl"


def log_files(path):
    backups = sorted(
        path.parent.glob(f"{path.name}.*"),
        key=lambda candidate: int(candidate.suffix[1:]) if candidate.suffix[1:].isdigit() else 0,
        reverse=True,
    )
    return [*backups, path]


def app_name(window):
    if not window:
        return "unknown"
    classes = window.get("wm_class") or []
    if classes:
        return str(classes[-1]).casefold()
    title = str(window.get("title") or "").strip()
    return title.casefold() if title else "unknown"


def top(counter, limit):
    return [{"name": name, "count": count} for name, count in counter.most_common(limit)]


def main():
    parser = argparse.ArgumentParser(description="Summarize Qtile JSONL telemetry for LLM analysis.")
    parser.add_argument("paths", nargs="*", type=Path, help="JSONL files; defaults to the Qtile telemetry log and rotations")
    parser.add_argument("--limit", type=int, default=50, help="Maximum entries per ranking")
    args = parser.parse_args()

    paths = args.paths or log_files(default_log_path())
    event_counts = Counter()
    keybinds = Counter()
    focused_apps = Counter()
    managed_apps = Counter()
    unmatched_apps = Counter()
    group_destinations = Counter()
    group_removals = Counter()
    layout_changes = Counter()
    auto_routes = Counter()
    group_views = Counter()
    screen_views = Counter()
    sessions = set()
    first_timestamp = None
    last_timestamp = None
    records = 0
    keymap = []

    for path in paths:
        if not path.exists():
            continue
        with path.open(encoding="utf-8") as stream:
            for line_number, line in enumerate(stream, 1):
                if not line.strip():
                    continue
                try:
                    record = json.loads(line)
                except json.JSONDecodeError as error:
                    raise SystemExit(f"{path}:{line_number}: invalid JSON: {error}") from error

                records += 1
                event = record.get("event", "unknown")
                timestamp = record.get("timestamp")
                event_counts[event] += 1
                if timestamp:
                    first_timestamp = min(first_timestamp, timestamp) if first_timestamp else timestamp
                    last_timestamp = max(last_timestamp, timestamp) if last_timestamp else timestamp
                if record.get("session"):
                    sessions.add(record["session"])

                if event == "session_start" and record.get("keymap"):
                    keymap = record["keymap"]
                elif event == "keybind":
                    keybinds[record.get("binding") or "unknown"] += 1
                elif event == "window_focus":
                    focused_apps[app_name(record.get("window"))] += 1
                elif event == "window_managed":
                    app = app_name(record.get("window"))
                    managed_apps[app] += 1
                    if not record.get("route_target"):
                        unmatched_apps[app] += 1
                elif event == "window_group_add":
                    group_destinations[(app_name(record.get("window")), record.get("group") or "unknown")] += 1
                elif event == "window_group_remove":
                    group_removals[(app_name(record.get("window")), record.get("group") or "unknown")] += 1
                elif event == "layout_change":
                    layout_changes[(record.get("group") or "unknown", record.get("layout") or "unknown")] += 1
                elif event == "window_auto_routed":
                    auto_routes[(
                        app_name(record.get("window")),
                        record.get("source_group") or "unknown",
                        record.get("target_group") or "unknown",
                    )] += 1
                elif event == "group_set":
                    group_views[record.get("group") or "unknown"] += 1
                elif event == "screen_change":
                    screen_views[str(record.get("screen"))] += 1

    def tuples(counter, labels):
        return [
            {**dict(zip(labels, values if isinstance(values, tuple) else (values,))), "count": count}
            for values, count in counter.most_common(args.limit)
        ]

    report = {
        "schema_version": 1,
        "source_files": [str(path) for path in paths if path.exists()],
        "records": records,
        "sessions": len(sessions),
        "first_timestamp": first_timestamp,
        "last_timestamp": last_timestamp,
        "event_counts": dict(event_counts.most_common()),
        "configured_keymap": keymap,
        "top_keybinds": top(keybinds, args.limit),
        "top_focused_apps": top(focused_apps, args.limit),
        "managed_apps": top(managed_apps, args.limit),
        "unmatched_apps": top(unmatched_apps, args.limit),
        "group_destinations": tuples(group_destinations, ("app", "group")),
        "group_removals": tuples(group_removals, ("app", "group")),
        "layout_changes": tuples(layout_changes, ("group", "layout")),
        "auto_routes": tuples(auto_routes, ("app", "source_group", "target_group")),
        "group_views": top(group_views, args.limit),
        "screen_views": top(screen_views, args.limit),
    }
    print(json.dumps(report, indent=2, sort_keys=True, ensure_ascii=False))


if __name__ == "__main__":
    main()
