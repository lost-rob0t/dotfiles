"""Generated from scripts/opencode-ses.org.  Python 3.10+, standard library only."""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import sys
from datetime import datetime

SESSION_ID = re.compile(r"ses_[A-Za-z0-9]+\Z")
LIST_PATH = "/api/session"
API_TIMEOUT = 60


class Failure(Exception):
    def __init__(self, message: str, code: int = 1):
        super().__init__(message)
        self.code = code


def executable(name: str) -> str:
    path = shutil.which(name)
    if path is None:
        raise Failure(f"required command not found: {name}", 127)
    return path


def listing(opencode: str) -> list[object]:
    """Return the raw session records from the supported OpenCode listing API."""
    try:
        result = subprocess.run([opencode, "api", "get", LIST_PATH],
                                capture_output=True, timeout=API_TIMEOUT)
    except subprocess.TimeoutExpired as error:
        raise Failure("opencode api timed out", 124) from error
    except OSError as error:
        raise Failure(f"could not run opencode api: {error}", 69) from error
    if result.returncode != 0:
        message = result.stderr.decode("utf-8", errors="replace").strip()
        raise Failure(f"listing OpenCode sessions: {message[:2048] or 'opencode api failed'}")
    try:
        payload = json.loads(result.stdout)
    except (ValueError, UnicodeError) as error:
        raise Failure("opencode api returned invalid JSON", 70) from error
    if not isinstance(payload, dict) or not isinstance(payload.get("data"), list):
        raise Failure("opencode api returned an unexpected session payload", 70)
    return payload["data"]


def parse_sessions(records: list[object]) -> list[dict]:
    """Keep well-formed records (id, directory, created/updated millis, title), newest first."""
    result = []
    for item in records:
        if not isinstance(item, dict):
            continue
        directory = None
        location = item.get("location")
        if isinstance(location, dict):
            directory = location.get("directory")
        created = updated = None
        times = item.get("time")
        if isinstance(times, dict):
            created, updated = times.get("created"), times.get("updated")
        title = item.get("title")
        if (isinstance(item.get("id"), str) and SESSION_ID.fullmatch(item["id"])
                and isinstance(directory, str) and directory.startswith("/")
                and isinstance(created, int) and not isinstance(created, bool)
                and isinstance(updated, int) and not isinstance(updated, bool)):
            result.append({"id": item["id"], "directory": directory,
                           "created": created, "updated": updated,
                           "title": title if isinstance(title, str) and title else "(untitled)"})
    result.sort(key=lambda session: (-session["updated"], session["id"]))
    return result


def rows(sessions: list[dict]) -> list[str]:
    def stamp(milliseconds: int) -> str:
        return datetime.fromtimestamp(milliseconds / 1000).strftime("%Y-%m-%d %H:%M")

    return [f"{stamp(session['updated'])}\tcreated {stamp(session['created'])}"
            f"\t{session['title']}\t{session['directory']}\t{session['id']}"
            for session in sessions]


def fzf_select(candidates: list[str]) -> str:
    env = os.environ.copy()
    env.update(FZF_DEFAULT_OPTS="", FZF_DEFAULT_OPTS_FILE="", FZF_DEFAULT_COMMAND="")
    # fzf owns the interaction and can remain open until the user chooses.
    result = subprocess.run([executable("fzf"), "--read0", "--print0", "--no-multi",
                             "--exit-0", "--height=40%", "--layout=reverse", "--prompt=OpenCode session> "],
                            input=("\0".join(candidates) + "\0").encode("utf-8"),
                            capture_output=True, env=env)
    if result.returncode in (1, 130) or (result.returncode == 0 and not result.stdout):
        raise Failure("", 130)
    if result.returncode != 0:
        message = result.stderr.decode("utf-8", errors="replace").strip()
        raise Failure(f"fzf: {message[:2048] or 'command failed'}")
    try:
        values = result.stdout.decode("utf-8").split("\0")
    except UnicodeError as error:
        raise Failure("fzf returned invalid UTF-8", 70) from error
    if len(values) != 2 or values[-1] != "" or values[0] not in candidates:
        raise Failure("fzf did not return exactly one original record", 70)
    return values[0]


def resume(opencode: str, session: dict) -> None:
    directory = Path(session["directory"])
    if not directory.is_dir():
        raise Failure(f"session directory no longer exists: {directory}")
    os.chdir(directory)
    # --session continues the exact session inside its own project directory.
    os.execv(opencode, [opencode, "--session", session["id"], session["directory"]])


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="opencode-ses",
        description="Pick an OpenCode session in fzf and resume it in its project directory.")
    parser.parse_args(argv)
    opencode = executable("opencode")
    executable("fzf")
    inventory = parse_sessions(listing(opencode))
    if not inventory:
        print("No OpenCode sessions. Start one with: opencode", file=sys.stderr)
        return 0
    display = rows(inventory)
    try:
        selected = fzf_select(display)
    except Failure as error:
        if error.code == 130:
            return 0
        raise
    session = inventory[display.index(selected)]
    # A vanished session must fail closed: --session would create a replacement.
    if session["id"] not in {live["id"] for live in parse_sessions(listing(opencode))}:
        raise Failure("selected session disappeared; run opencode-ses again")
    resume(opencode, session)
    return 0


def interrupted(signum: int, _frame: object) -> None:
    raise Failure("", 128 + signum)


if __name__ == "__main__":
    for termination_signal in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(termination_signal, interrupted)
    try:
        status = main()
    except Failure as error:
        if str(error):
            print(f"{Path(sys.argv[0]).stem}: {error}", file=sys.stderr)
        status = error.code
    except KeyboardInterrupt:
        status = 130
    except BrokenPipeError:
        # Avoid an additional flush exception while the interpreter shuts down.
        os.dup2(os.open(os.devnull, os.O_WRONLY), sys.stdout.fileno())
        status = 141
    except (OSError, UnicodeError) as error:
        print(f"opencode-ses: {error}", file=sys.stderr)
        status = 1
    sys.exit(status)
