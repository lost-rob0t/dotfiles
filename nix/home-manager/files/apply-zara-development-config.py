#!/usr/bin/env python3
"""Merge only Zara task/coding setup; retain user-owned TOML and credentials."""
from __future__ import annotations

import json
import math
import os
from pathlib import Path
import stat
import sys
import tempfile
from collections.abc import MutableMapping

import tomlkit

TASK_KEYS = {"enabled", "max_concurrent", "max_task_steps", "wall_clock_minutes", "step_log_chars"}
CODING_KEYS = {"allowed_roots", "prolog_rlm_checkout", "git", "swipl"}


def _path(value: object) -> bool:
    return isinstance(value, str) and value.startswith("/") and len(value) <= 4096 and not any(c in value for c in "\0\r\n")


def validate_settings(settings: object) -> dict:
    if not isinstance(settings, dict) or not settings or set(settings) - {"tasks", "plugins"}:
        raise ValueError("only task and zara-coding settings may be managed")
    if "tasks" in settings:
        tasks = settings["tasks"]
        if not isinstance(tasks, dict) or set(tasks) != TASK_KEYS or type(tasks["enabled"]) is not bool:
            raise ValueError("invalid task settings")
        for key in ("max_concurrent", "max_task_steps", "step_log_chars"):
            if type(tasks[key]) is not int or tasks[key] <= 0:
                raise ValueError(f"{key} must be a positive integer")
        wall = tasks["wall_clock_minutes"]
        if type(wall) not in (int, float) or not math.isfinite(wall) or wall <= 0:
            raise ValueError("wall_clock_minutes must be finite and positive")
    if "plugins" in settings:
        plugins = settings["plugins"]
        if not isinstance(plugins, dict) or set(plugins) != {"zara-coding"}:
            raise ValueError("only zara-coding plugin settings may be managed")
        coding = plugins["zara-coding"]
        if not isinstance(coding, dict) or set(coding) != CODING_KEYS:
            raise ValueError("invalid coding settings")
        roots = coding["allowed_roots"]
        if not isinstance(roots, list) or not 1 <= len(roots) <= 64:
            raise ValueError("configure 1 through 64 repository roots")
        if any(not _path(root) or Path(root) == Path("/") for root in roots):
            raise ValueError("repository roots must be bounded absolute paths, not /")
        if len(set(roots)) != len(roots):
            raise ValueError("repository roots must be unique")
        if any(not _path(coding[key]) for key in ("prolog_rlm_checkout", "git", "swipl")):
            raise ValueError("coding runtime paths must be absolute")
    return settings


def _merge(document: MutableMapping, settings: dict) -> None:
    for key, value in settings.items():
        if isinstance(value, dict):
            if key not in document:
                document[key] = tomlkit.table()
            table = document[key]
            if not isinstance(table, MutableMapping):
                raise ValueError(f"{key} must be a TOML table")
            _merge(table, value)
        else:
            document[key] = value


def _identity(path: Path):
    try:
        info = path.lstat()
    except FileNotFoundError:
        return None
    if not stat.S_ISREG(info.st_mode):
        raise ValueError("config.toml must be a regular, user-owned file")
    return info.st_dev, info.st_ino, info.st_size, info.st_mtime_ns, info.st_ctime_ns


def apply_config(path: Path, settings: dict) -> bool:
    """Return whether bytes changed. No scheduler or coding task is executed."""
    validate_settings(settings)
    identity = _identity(path)  # Reject live and dangling symlinks before reading.
    original = ""
    if identity is not None:
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
        with os.fdopen(fd, "r", encoding="utf-8") as stream:
            if not stat.S_ISREG(os.fstat(stream.fileno()).st_mode):
                raise ValueError("config.toml must be a regular file")
            original = stream.read(4 * 1024 * 1024 + 1)
        if len(original) > 4 * 1024 * 1024:
            raise ValueError("config.toml exceeds the supported size")
    document = tomlkit.parse(original)
    _merge(document, settings)
    rendered = tomlkit.dumps(document)
    if rendered == original:
        return False
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix=".zara-development-", dir=path.parent)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            stream.write(rendered)
            stream.flush()
            os.fsync(stream.fileno())
        os.chmod(temporary, 0o600)
        if _identity(path) != identity:
            raise RuntimeError("config.toml changed during activation; retry without overwriting it")
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)
    return True


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: apply-zara-development-config CONFIG_TOML SETTINGS_JSON", file=sys.stderr)
        return 2
    try:
        settings = json.loads(Path(sys.argv[2]).read_text(encoding="utf-8"))
        apply_config(Path(sys.argv[1]).expanduser(), settings)
    except (OSError, ValueError, RuntimeError):
        # TOML exceptions can contain source lines; never echo mutable config.
        print("Zara setup was not applied; check paths and TOML table types. Existing config retained.", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
