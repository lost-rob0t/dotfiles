#!/usr/bin/env python3

from __future__ import annotations

import argparse
import os
import re
import shlex
import shutil
import stat
import sys
import time
from dataclasses import dataclass
from pathlib import Path


TIMESTAMP_RE = re.compile(r"^#\d{9,}$")

SECRET_PATTERNS = [
    # Generic assignments / flags.
    re.compile(
        r"""(?ix)
        \b(
            password|passwd|pwd|
            api[_-]?key|api[_-]?token|
            access[_-]?token|auth[_-]?token|
            bearer[_-]?token|
            client[_-]?secret|
            secret[_-]?key|
            private[_-]?key
        )
        \s*(?:=|:)\s*
        ['"]?[^\s'"]+
        """
    ),

    # CLI flags.
    re.compile(
        r"""(?ix)
        --(
            password|passwd|
            api-key|api_key|
            api-token|api_token|
            access-token|access_token|
            token|
            client-secret|client_secret
        )
        (?:=|\s+)\S+
        """
    ),

    # Authorization headers.
    re.compile(r"(?i)\bauthorization\s*:\s*(?:bearer|basic)\s+\S+"),

    # Common token formats.
    re.compile(r"\bgh[pousr]_[A-Za-z0-9_]{20,}\b"),
    re.compile(r"\bgithub_pat_[A-Za-z0-9_]{20,}\b"),
    re.compile(r"\bsk-[A-Za-z0-9_-]{20,}\b"),
    re.compile(r"\bxox[baprs]-[A-Za-z0-9-]{10,}\b"),
    re.compile(r"\bAKIA[0-9A-Z]{16}\b"),

    # JWTs.
    re.compile(
        r"\beyJ[A-Za-z0-9_-]{5,}\."
        r"[A-Za-z0-9_-]{5,}\."
        r"[A-Za-z0-9_-]{5,}\b"
    ),

    # Credentials embedded in URLs.
    re.compile(r"(?i)\b[a-z][a-z0-9+.-]*://[^/\s:@]+:[^/\s@]+@"),

    # Sensitive env vars even if token format is unknown.
    re.compile(
        r"""(?ix)
        \b(
            OPENAI_API_KEY|
            ANTHROPIC_API_KEY|
            OPENROUTER_API_KEY|
            GITHUB_TOKEN|
            GH_TOKEN|
            AWS_SECRET_ACCESS_KEY|
            AWS_SESSION_TOKEN|
            DATABASE_URL
        )=
        """
    ),
]

SHELLISH_RE = [
    re.compile(r"^\s*[A-Za-z_][A-Za-z0-9_]*="),
    re.compile(r"^\s*(?:if|for|while|until|case|select|function)\b"),
    re.compile(r"^\s*(?:cd|pushd|popd)\b"),
    re.compile(r"^\s*(?:\.|source)\s+"),
    re.compile(r"^\s*[./~][^\s]*"),
    re.compile(r"[|;&<>`]"),
    re.compile(r"\$\("),
    re.compile(r"\$\{"),
]

PROSE_HINTS = re.compile(
    r"""(?ix)
    \b(
        the|this|that|these|those|
        please|because|should|would|could|
        what|why|where|when|who|how|
        actually|basically|
        here's|here's|dont|don't|
        can\s+you|i\s+want|i\s+need
    )\b
    """
)


@dataclass(frozen=True)
class Entry:
    timestamp: str | None
    command: str


def read_entries(path: Path) -> list[Entry]:
    lines = path.read_text(errors="replace").splitlines()

    entries: list[Entry] = []
    timestamp: str | None = None

    for line in lines:
        if TIMESTAMP_RE.fullmatch(line):
            timestamp = line
            continue

        if not line.strip():
            continue

        entries.append(Entry(timestamp, line))
        timestamp = None

    return entries


def secret_reason(command: str) -> str | None:
    for pattern in SECRET_PATTERNS:
        if pattern.search(command):
            return pattern.pattern[:50]

    return None


def command_names() -> set[str]:
    names = {
        "!",
        ".",
        ":",
        "[",
        "[[",
        "alias",
        "bg",
        "break",
        "builtin",
        "caller",
        "cd",
        "command",
        "compgen",
        "complete",
        "continue",
        "declare",
        "dirs",
        "disown",
        "echo",
        "enable",
        "eval",
        "exec",
        "exit",
        "export",
        "false",
        "fc",
        "fg",
        "getopts",
        "hash",
        "help",
        "history",
        "jobs",
        "kill",
        "let",
        "local",
        "logout",
        "mapfile",
        "printf",
        "pushd",
        "pwd",
        "read",
        "readonly",
        "return",
        "set",
        "shift",
        "shopt",
        "source",
        "suspend",
        "test",
        "times",
        "trap",
        "true",
        "type",
        "typeset",
        "ulimit",
        "umask",
        "unalias",
        "unset",
        "wait",
    }

    for directory in os.environ.get("PATH", "").split(os.pathsep):
        try:
            for entry in Path(directory).iterdir():
                if entry.is_file() and os.access(entry, os.X_OK):
                    names.add(entry.name)
        except (OSError, PermissionError):
            pass

    return names


def first_word(command: str) -> str | None:
    try:
        words = shlex.split(command, comments=False, posix=True)
    except ValueError:
        return None

    if not words:
        return None

    # Skip leading env assignments.
    index = 0
    while index < len(words):
        word = words[index]
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*=.*", word):
            index += 1
            continue

        break

    if index >= len(words):
        return None

    return words[index]


def looks_like_shell(command: str, commands: set[str]) -> bool:
    stripped = command.strip()

    if not stripped:
        return False

    for pattern in SHELLISH_RE:
        if pattern.search(stripped):
            return True

    word = first_word(stripped)
    if word is None:
        return False

    if word in {"sudo", "doas", "env", "command", "nohup", "time"}:
        return True

    basename = Path(word).name

    if basename in commands:
        return True

    # Long natural-language entries are usually accidental pastes.
    words = stripped.split()

    if len(words) >= 8 and PROSE_HINTS.search(stripped):
        return False

    # Ambiguous short entry: preserve instead of destroying potentially
    # useful history.
    return len(words) <= 5


def clean(
    entries: list[Entry],
    commands: set[str],
) -> tuple[list[Entry], list[tuple[str, Entry]]]:
    kept_rev: list[Entry] = []
    rejected: list[tuple[str, Entry]] = []
    seen: set[str] = set()

    # Walk newest -> oldest so deduplication preserves the most recent use.
    for entry in reversed(entries):
        command = entry.command.strip()

        secret = secret_reason(command)
        if secret:
            rejected.append(("secret", entry))
            continue

        if not looks_like_shell(command, commands):
            rejected.append(("non-shell", entry))
            continue

        if command in seen:
            rejected.append(("duplicate", entry))
            continue

        seen.add(command)
        kept_rev.append(entry)

    return list(reversed(kept_rev)), list(reversed(rejected))


def write_history(path: Path, entries: list[Entry]) -> None:
    with path.open("w") as stream:
        for entry in entries:
            if entry.timestamp:
                stream.write(entry.timestamp)
                stream.write("\n")

            stream.write(entry.command)
            stream.write("\n")


def write_quarantine(
    path: Path,
    rejected: list[tuple[str, Entry]],
) -> None:
    with path.open("w") as stream:
        for reason, entry in rejected:
            stream.write(f"[{reason}] {entry.command}\n")

    path.chmod(stat.S_IRUSR | stat.S_IWUSR)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Conservatively sanitize ~/.bash_history."
    )
    parser.add_argument(
        "--history",
        type=Path,
        default=Path.home() / ".bash_history",
    )
    parser.add_argument(
        "--apply",
        action="store_true",
        help="replace the original history after creating a backup",
    )

    args = parser.parse_args()
    history = args.history.expanduser()

    if not history.exists():
        print(f"history does not exist: {history}", file=sys.stderr)
        return 1

    entries = read_entries(history)
    cleaned, rejected = clean(entries, command_names())

    stamp = time.strftime("%Y%m%d-%H%M%S")
    preview = history.with_name(f"{history.name}.cleaned-{stamp}")
    quarantine = history.with_name(f"{history.name}.quarantine-{stamp}")

    write_history(preview, cleaned)
    write_quarantine(quarantine, rejected)

    counts: dict[str, int] = {}
    for reason, _ in rejected:
        counts[reason] = counts.get(reason, 0) + 1

    print(f"original:    {len(entries):7d}")
    print(f"kept:        {len(cleaned):7d}")
    print(f"duplicates:  {counts.get('duplicate', 0):7d}")
    print(f"secrets:     {counts.get('secret', 0):7d}")
    print(f"non-shell:   {counts.get('non-shell', 0):7d}")
    print()
    print(f"preview:     {preview}")
    print(f"quarantine:  {quarantine}")

    if not args.apply:
        print()
        print("Dry run only. Review the two files, then rerun with --apply.")
        return 0

    backup = history.with_name(f"{history.name}.backup-{stamp}")
    shutil.copy2(history, backup)
    shutil.copy2(preview, history)
    history.chmod(stat.S_IRUSR | stat.S_IWUSR)

    print()
    print(f"backup:      {backup}")
    print(f"installed:   {history}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
