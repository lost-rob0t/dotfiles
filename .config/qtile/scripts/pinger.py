#!/usr/bin/env python3
"""Host aliveness monitor for the qtile autostart session.

Reads host definitions from ~/.config/hosts.toml (TOML) and, on a fixed
interval, runs either an ICMP ping or a non-interactive SSH probe against
each entry. Hosts that fail their probe produce a dunstify critical
notification. Designed to run forever as a background process; safe to
re-launch because each instance is independent.
"""

from __future__ import annotations

import argparse
import os
import shlex
import subprocess
import sys
import time
import tomllib
from dataclasses import dataclass
from pathlib import Path

DEFAULT_CONFIG = Path(os.environ.get("HOME", "/home/unseen")) / ".config" / "hosts.toml"
DEFAULT_INTERVAL = 30
DEFAULT_TIMEOUT = 5
DEFAULT_SSH_PORT = 22
NOTIFIER = "dunstify"
NOTIFIER_APP = "Pinger"


@dataclass(frozen=True)
class Host:
    name: str
    address: str
    method: str = "ping"
    user: str | None = None
    port: int = DEFAULT_SSH_PORT
    key: str | None = None
    timeout: int = DEFAULT_TIMEOUT


def load_hosts(path: Path) -> tuple[int, list[Host]]:
    """Parse the TOML config file into an interval and a list of Hosts."""
    with path.open("rb") as fh:
        data = tomllib.load(fh)
    interval = int(data.get("interval_seconds", DEFAULT_INTERVAL))
    if interval <= 0:
        raise ValueError(f"interval_seconds must be positive, got {interval}")
    hosts: list[Host] = []
    for raw in data.get("hosts", []):
        name = raw.get("name")
        address = raw.get("address") or raw.get("host") or raw.get("hostname")
        if not name or not address:
            raise ValueError(
                f"each host needs a 'name' and an 'address' (or 'host'/'hostname'); got {raw!r}"
            )
        method = str(raw.get("method", "ping")).lower()
        if method not in ("ping", "ssh"):
            raise ValueError(f"unsupported method {method!r} for host {name!r}")
        port = int(raw.get("port", DEFAULT_SSH_PORT))
        timeout = int(raw.get("timeout", DEFAULT_TIMEOUT))
        hosts.append(
            Host(
                name=name,
                address=address,
                method=method,
                user=raw.get("user"),
                port=port,
                key=raw.get("key"),
                timeout=timeout,
            )
        )
    return interval, hosts


def _run(cmd: list[str], timeout: int) -> bool:
    """Run a command, returning True on exit 0."""
    try:
        result = subprocess.run(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return False
    except FileNotFoundError:
        return False
    return result.returncode == 0


def ping_command(host: Host) -> list[str]:
    # One packet, no DNS, don't wait for the full timeout if it replies.
    return [
        "ping",
        "-c", "1",
        "-W", str(host.timeout),
        host.address,
    ]


def ssh_command(host: Host) -> list[str]:
    cmd = [
        "ssh",
        "-o", "BatchMode=yes",
        "-o", f"ConnectTimeout={host.timeout}",
        "-o", "StrictHostKeyChecking=accept-new",
        "-p", str(host.port),
    ]
    if host.key:
        cmd += ["-i", host.key]
    target = f"{host.user}@{host.address}" if host.user else host.address
    cmd += [target, "true"]
    return cmd


def probe(host: Host) -> bool:
    if host.method == "ssh":
        return _run(ssh_command(host), host.timeout + 2)
    return _run(ping_command(host), host.timeout + 2)


def notify(host: Host, down: bool) -> None:
    msg = f"{host.name} ({host.address}) is {'DOWN' if down else 'back up'}"
    urgency = "critical" if down else "normal"
    notifier = _which(NOTIFIER) or _which("notify-send")
    if not notifier:
        print(msg, file=sys.stderr)
        return
    try:
        subprocess.run(
            [notifier, "-a", NOTIFIER_APP, "-u", urgency, msg],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except FileNotFoundError:
        print(msg, file=sys.stderr)


def _which(name: str) -> str | None:
    path = os.environ.get("PATH", "")
    for entry in path.split(os.pathsep):
        candidate = Path(entry) / name
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return str(candidate)
    return None


def sweep(hosts: list[Host], state: dict[str, bool]) -> None:
    """Probe every host once; flip notifications on state transitions."""
    for host in hosts:
        ok = probe(host)
        was_up = state.get(host.name, True)
        if not ok and was_up:
            notify(host, down=True)
        elif ok and not was_up:
            notify(host, down=False)
        state[host.name] = ok


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-c", "--config",
        default=str(DEFAULT_CONFIG),
        help=f"path to hosts.toml (default: {DEFAULT_CONFIG})",
    )
    parser.add_argument(
        "-o", "--once",
        action="store_true",
        help="run a single sweep and exit (for testing)",
    )
    parser.add_argument(
        "-i", "--interval",
        type=int,
        default=None,
        help="override interval_seconds from config",
    )
    args = parser.parse_args(argv)

    config_path = Path(args.config)
    if not config_path.is_file():
        print(f"config not found: {config_path}", file=sys.stderr)
        return 2
    try:
        interval, hosts = load_hosts(config_path)
    except (tomllib.TOMLDecodeError, ValueError, KeyError) as exc:
        print(f"invalid config {config_path}: {exc}", file=sys.stderr)
        return 2
    if not hosts:
        print(f"no hosts configured in {config_path}", file=sys.stderr)
        return 2
    if args.interval is not None:
        interval = args.interval

    state: dict[str, bool] = {h.name: True for h in hosts}
    while True:
        sweep(hosts, state)
        if args.once:
            return 0
        time.sleep(interval)


if __name__ == "__main__":
    sys.exit(main())
