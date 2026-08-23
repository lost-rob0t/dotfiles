#!/usr/bin/env python3
"""Qtile-friendly Dunst notification history/menu helper."""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
from typing import Any


def _run(command: list[str], *, stdin: str | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        input=stdin,
        text=True,
        capture_output=True,
        check=False,
        timeout=10,
    )


def _typed(item: dict[str, Any], key: str, default: Any = "") -> Any:
    value = item.get(key, {})
    if isinstance(value, dict):
        return value.get("data", default)
    return default


def history_entries(payload: dict[str, Any]) -> list[dict[str, Any]]:
    """Flatten dunstctl's aa{sv} JSON envelope into useful records."""
    entries: list[dict[str, Any]] = []
    for group in payload.get("data", []):
        if not isinstance(group, list):
            continue
        for item in group:
            if not isinstance(item, dict):
                continue
            try:
                identifier = int(_typed(item, "id", -1))
            except (TypeError, ValueError):
                continue
            if identifier < 0:
                continue
            entries.append(
                {
                    "id": identifier,
                    "app": str(_typed(item, "appname", "")),
                    "summary": str(_typed(item, "summary", "")),
                    "body": str(_typed(item, "body", "")),
                }
            )
    return entries


def _history() -> list[dict[str, Any]]:
    completed = _run(["dunstctl", "history"])
    if completed.returncode != 0:
        return []
    try:
        payload = json.loads(completed.stdout)
    except json.JSONDecodeError:
        return []
    return history_entries(payload) if isinstance(payload, dict) else []


def _paused() -> bool:
    completed = _run(["dunstctl", "is-paused"])
    return completed.returncode == 0 and completed.stdout.strip().lower() == "true"


def status_text() -> str:
    count = _run(["dunstctl", "count", "history"])
    amount = count.stdout.strip() if count.returncode == 0 else "?"
    return f"{'' if _paused() else ''} {amount or '0'}"


def _clean(value: str, limit: int = 100) -> str:
    value = " ".join(value.replace("\n", " ").split())
    return value if len(value) <= limit else value[: limit - 1] + "…"


def menu_rows(entries: list[dict[str, Any]], paused: bool) -> list[str]:
    rows = [
        f" DND: {'ON' if paused else 'OFF'}  — toggle",
        "󰆴 Clear notification history",
    ]
    for entry in entries:
        app = _clean(entry["app"], 24) or "notification"
        summary = _clean(entry["summary"], 55)
        body = _clean(entry["body"], 70)
        detail = summary or body or "(empty)"
        if body and summary and body != summary:
            detail += f" — {body}"
        rows.append(f"#{entry['id']}  {app}: {detail}")
    return rows


def _dmenu(rows: list[str]) -> str | None:
    executable = shutil.which("dmenu")
    if not executable:
        return None
    completed = _run(
        [executable, "-i", "-l", "20", "-p", "Notifications:"],
        stdin="\n".join(rows) + "\n",
    )
    if completed.returncode != 0:
        return None
    selected = completed.stdout.strip()
    return selected or None


def show_menu() -> int:
    entries = _history()
    selected = _dmenu(menu_rows(entries, _paused()))
    if not selected:
        return 0
    if selected.startswith(" DND:"):
        return _run(["dunstctl", "set-paused", "toggle"]).returncode
    if selected.startswith("󰆴 Clear"):
        return _run(["dunstctl", "history-clear"]).returncode
    if selected.startswith("#"):
        token = selected.split(maxsplit=1)[0][1:]
        if token.isdigit():
            return _run(["dunstctl", "history-pop", token]).returncode
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--status", action="store_true")
    mode.add_argument("--menu", action="store_true")
    args = parser.parse_args()
    if args.status:
        print(status_text())
        return 0
    return show_menu()


if __name__ == "__main__":
    raise SystemExit(main())
