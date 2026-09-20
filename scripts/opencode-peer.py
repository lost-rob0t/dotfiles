#!/usr/bin/env python3
"""Generated from lisp/llm/opencode-workspace.org. Same-user JSON/emacsclient bridge."""
from __future__ import annotations

import argparse
import base64
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import uuid

MAX_TEXT = 16_384
NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]{0,63}\Z")


def envelope(target: str, text: str, environment: dict[str, str]) -> dict[str, object]:
    """Build data only; the receiving Emacs validates the live sender generation."""
    sender = environment.get("OPENCODE_EMACS_AGENT", "")
    workspace = environment.get("OPENCODE_EMACS_WORKSPACE", "")
    generation = environment.get("OPENCODE_EMACS_GENERATION", "")
    if not NAME.fullmatch(target) or not NAME.fullmatch(sender) or target == sender:
        raise ValueError("invalid target/sender or self-addressed message")
    if not re.fullmatch(r"[A-Za-z0-9_-]{1,128}", workspace):
        raise ValueError("missing or invalid workspace identity")
    if not re.fullmatch(r"[0-9]{1,16}", generation):
        raise ValueError("missing or invalid worker generation")
    if not text or len(text.encode("utf-8")) > MAX_TEXT:
        raise ValueError("message must contain 1-16384 UTF-8 bytes")
    return {"version": 1, "workspace": workspace, "id": uuid.uuid4().hex,
            "from": sender, "to": target, "generation": int(generation), "text": text}


def deliver(packet: dict[str, object], *, client: str = "emacsclient",
            server: str = "server") -> None:
    """Send a private file reference. Never interpolate user text into Lisp or shell."""
    with tempfile.TemporaryDirectory(prefix="opencode-peer-") as directory:
        os.chmod(directory, 0o700)
        path = Path(directory) / "message.json"
        descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(packet, stream, ensure_ascii=False)
        encoded = base64.b64encode(str(path).encode("utf-8")).decode("ascii")
        expression = (
            '(opencode-workspace-receive-file '
            f'(decode-coding-string (base64-decode-string "{encoded}") \'utf-8))'
        )
        # Do not auto-start Emacs or silently deliver to a different server.
        completed = subprocess.run(
            [client, "--socket-name", server, "--eval", expression],
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
            timeout=15, check=False, text=True, encoding="utf-8",
        )
        if completed.returncode != 0 or completed.stdout.strip() != "t":
            raise RuntimeError("Emacs did not acknowledge the peer message")


def main() -> int:
    parser = argparse.ArgumentParser(description="Send stdin to a registered Emacs OpenCode peer")
    parser.add_argument("target")
    args = parser.parse_args()
    try:
        raw = sys.stdin.buffer.read(MAX_TEXT + 1)
        if len(raw) > MAX_TEXT:
            raise ValueError("message exceeds 16384 bytes")
        packet = envelope(args.target, raw.decode("utf-8"), dict(os.environ))
        deliver(packet, server=os.environ.get("OPENCODE_EMACS_SERVER", "server"))
    except (OSError, ValueError, RuntimeError, subprocess.TimeoutExpired) as error:
        print(f"opencode-peer: {error}", file=sys.stderr)
        return 1
    print("accepted (queued; not a completion receipt)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
