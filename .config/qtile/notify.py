"""Shared non-blocking desktop notification helper for Qtile widgets."""

from __future__ import annotations

import shutil
import subprocess


def notify(summary: str, body: str = "", *, urgency: str = "low", app: str = "Qtile") -> None:
    """Send a desktop notification without ever blocking the caller."""
    notifier = shutil.which("dunstify") or shutil.which("notify-send")
    if not notifier:
        return
    command = [notifier, "-a", app, "-u", urgency, summary]
    if body:
        command.append(body)
    try:
        subprocess.Popen(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except OSError:
        pass
