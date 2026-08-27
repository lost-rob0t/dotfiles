"""Persistent Qtile UI presentation settings."""

from __future__ import annotations

import json
import os
import tempfile
from pathlib import Path
from typing import Any


DEFAULT_UI_SETTINGS = {"notification_ui": "dmenu"}
VALID_NOTIFICATION_UIS = {"dmenu", "emacs"}
UI_SETTINGS_PATH = Path("~/.config/qtile/ui-settings.json").expanduser()


def load_ui_settings(path: Path = UI_SETTINGS_PATH) -> dict[str, Any]:
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError, TypeError):
        return dict(DEFAULT_UI_SETTINGS)
    if not isinstance(payload, dict):
        return dict(DEFAULT_UI_SETTINGS)
    settings = dict(DEFAULT_UI_SETTINGS)
    settings.update({key: value for key, value in payload.items() if isinstance(key, str)})
    if settings.get("notification_ui") not in VALID_NOTIFICATION_UIS:
        settings["notification_ui"] = DEFAULT_UI_SETTINGS["notification_ui"]
    return settings


def save_ui_settings(settings: dict[str, Any], path: Path = UI_SETTINGS_PATH) -> None:
    normalized = dict(DEFAULT_UI_SETTINGS)
    normalized.update(settings)
    if normalized.get("notification_ui") not in VALID_NOTIFICATION_UIS:
        raise ValueError("notification_ui must be dmenu or emacs")
    path = Path(path).expanduser()
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(normalized, stream, indent=2, sort_keys=True)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except OSError:
            pass
        raise


def get_notification_ui(path: Path = UI_SETTINGS_PATH) -> str:
    return str(load_ui_settings(path).get("notification_ui", "dmenu"))


def set_notification_ui(value: str, path: Path = UI_SETTINGS_PATH) -> str:
    if value not in VALID_NOTIFICATION_UIS:
        raise ValueError("notification_ui must be dmenu or emacs")
    settings = load_ui_settings(path)
    settings["notification_ui"] = value
    save_ui_settings(settings, path)
    return value


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--get", action="store_true")
    mode.add_argument("--set", dest="value", choices=sorted(VALID_NOTIFICATION_UIS))
    arguments = parser.parse_args()
    if arguments.get:
        print(get_notification_ui())
    else:
        print(set_notification_ui(arguments.value))
