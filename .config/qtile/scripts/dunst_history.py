#!/usr/bin/env python3
"""Normalize Dunst history for native menus and Emacs dashboards."""

from __future__ import annotations

import argparse
import json
import subprocess
from typing import Any


def run(command: list[str], *, stdin: str | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        input=stdin,
        text=True,
        capture_output=True,
        check=False,
        timeout=10,
    )


def typed(item: dict[str, Any], key: str, default: Any = "") -> Any:
    value = item.get(key, {})
    if isinstance(value, dict):
        return value.get("data", default)
    return default


def history_entries(payload: dict[str, Any]) -> list[dict[str, Any]]:
    """Flatten every record in dunstctl's aa{sv} history envelope."""
    entries: list[dict[str, Any]] = []
    for group in payload.get("data", []):
        if not isinstance(group, list):
            continue
        for item in group:
            if not isinstance(item, dict):
                continue
            try:
                identifier = int(typed(item, "id", -1))
            except (TypeError, ValueError):
                continue
            if identifier < 0:
                continue
            urgency = str(typed(item, "urgency", "normal")).casefold()
            if urgency not in {"low", "normal", "critical"}:
                urgency = "normal"
            entries.append(
                {
                    "id": identifier,
                    "app": str(typed(item, "appname", "")),
                    "summary": str(typed(item, "summary", "")),
                    "body": str(typed(item, "body", "")),
                    "urgency": urgency,
                    "timestamp": str(typed(item, "timestamp", "")),
                    "category": str(typed(item, "category", "")),
                }
            )
    return entries


def history() -> list[dict[str, Any]]:
    return _history_with_error()[0]


def _history_with_error() -> tuple[list[dict[str, Any]], str | None]:
    completed = run(["dunstctl", "history"])
    if completed.returncode != 0:
        return [], "dunstctl history failed"
    try:
        payload = json.loads(completed.stdout)
    except json.JSONDecodeError:
        return [], "dunstctl history returned invalid JSON"
    if not isinstance(payload, dict):
        return [], "dunstctl history returned an invalid payload"
    return history_entries(payload), None


def paused() -> bool:
    return _paused_with_error()[0]


def _paused_with_error() -> tuple[bool, str | None]:
    completed = run(["dunstctl", "is-paused"])
    if completed.returncode != 0:
        return False, "dunstctl is-paused failed"
    return completed.stdout.strip().lower() == "true", None


def snapshot() -> dict[str, Any]:
    entries, history_error = _history_with_error()
    paused_state, paused_error = _paused_with_error()
    errors = [error for error in (history_error, paused_error) if error]
    return {
        "entries": entries,
        "paused": paused_state,
        "error": "; ".join(errors) if errors else None,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--json", action="store_true", required=True)
    parser.parse_args()
    print(json.dumps(snapshot(), ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
