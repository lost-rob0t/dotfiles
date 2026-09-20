#!/usr/bin/env python3
"""ZARA-CREW/1 typed local client; transport reuses the existing private-file bridge."""
from __future__ import annotations

import argparse
import importlib.util
import json
import os
from pathlib import Path
import re
import sys
import uuid

NAME = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]{0,63}\Z")
PROTOCOL = "ZARA-CREW/1"


def build(request: dict, env: dict[str, str]) -> dict:
    if not isinstance(request, dict) or set(request) - {"action", "target", "role", "text", "turns", "request_id"}:
        raise ValueError("unknown request fields")
    action, text = request.get("action"), request.get("text")
    if action not in {"message", "spawn"} or not isinstance(text, str) or not 0 < len(text.encode()) <= 8192:
        raise ValueError("invalid action or text size")
    identity = {}
    for key, variable in [("from", "OPENCODE_EMACS_AGENT"), ("crew", "OPENCODE_CREW_NAME"),
                          ("workspace", "OPENCODE_EMACS_WORKSPACE"), ("run", "OPENCODE_CREW_RUN")]:
        value = env.get(variable, "")
        if not NAME.fullmatch(value):
            raise ValueError(f"missing/invalid host identity: {key}")
        identity[key] = value
    for key, variable in [("generation", "OPENCODE_EMACS_GENERATION"), ("epoch", "OPENCODE_CREW_EPOCH")]:
        value = env.get(variable, "")
        if not re.fullmatch(r"[0-9]{1,16}", value):
            raise ValueError(f"invalid host generation: {key}")
        identity[key] = int(value)
    request_id = request.get("request_id", uuid.uuid4().hex)
    if not isinstance(request_id, str) or not NAME.fullmatch(request_id):
        raise ValueError("invalid idempotency key")
    packet = dict(identity, protocol=PROTOCOL, version=1, id=request_id, method=action, text=text)
    if action == "message":
        target = request.get("target")
        if not isinstance(target, str) or not NAME.fullmatch(target) or target == identity["from"]:
            raise ValueError("invalid/self-addressed target")
        if "role" in request or "turns" in request:
            raise ValueError("message must not contain spawn fields")
        packet["to"] = target
    else:
        role, turns = request.get("role"), request.get("turns", 4)
        if not isinstance(role, str) or not NAME.fullmatch(role) or type(turns) is not int or not 0 < turns <= 100000:
            raise ValueError("invalid child role/turn allocation")
        if "target" in request:
            raise ValueError("spawn target is coordinator-owned")
        packet.update(role=role, turns=turns)
    return packet


def no_duplicates(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON key")
        result[key] = value
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("target", nargs="?")
    parser.add_argument("--request", action="store_true", help="typed JSON request on stdin")
    parser.add_argument("--spawn", metavar="ROLE")
    parser.add_argument("--turns", type=int, default=4)
    args = parser.parse_args()
    raw = sys.stdin.buffer.read(16385)
    try:
        if len(raw) > 16384:
            raise ValueError("request exceeds 16 KiB")
        if args.request:
            request = json.loads(raw, object_pairs_hook=no_duplicates)
        elif args.spawn:
            request = {"action": "spawn", "role": args.spawn, "turns": args.turns, "text": raw.decode()}
        else:
            request = {"action": "message", "target": args.target, "text": raw.decode()}
        packet = build(request, dict(os.environ))
        source = Path(__file__).with_name("opencode-peer.py")
        spec = importlib.util.spec_from_file_location("crew_private_transport", source)
        transport = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(transport)
        transport.deliver(packet, server=os.environ.get("OPENCODE_EMACS_SERVER", "server"))
    except Exception as error:
        # Do not echo private message contents or environment values.
        print(f"crew-client: {type(error).__name__}: request not acknowledged", file=sys.stderr)
        return 1
    print(json.dumps({"protocol": PROTOCOL, "request_id": packet["id"], "accepted": True,
                      "state": "policy_pending" if packet["method"] == "spawn" else "queued"}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
