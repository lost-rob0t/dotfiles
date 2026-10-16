#!/usr/bin/env python3
"""Exercise evaluated dependency/restart wiring under a disposable CI user manager.

The packages and secret-dependent commands are replaced with lifecycle probes.
This verifies systemd behavior, not actual StarIntel ingestion or readiness APIs.
Never run under a real operator account: CI creates a new dedicated user.
"""
import json
import os
from pathlib import Path
import pwd
import shlex
import subprocess
import sys
import tempfile
import time


def systemctl(*args):
    if args[0] != "show":
        print("systemctl --user " + " ".join(args), flush=True)
    return subprocess.check_output(["systemctl", "--user", *args], text=True,
                                   timeout=45).strip()


def wait_for(predicate, description):
    deadline = time.monotonic() + 40
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(0.2)
    raise RuntimeError("Timed out: " + description)


def probe(name, root):
    root = Path(root)
    required = {
        "starintel-infrastructure": ["starintel-prepare"],
        "starintel-server": ["starintel-infrastructure"],
        "starintel-quasar": ["starintel-server"],
        "starintel-actor-fixture": ["starintel-server"],
        "starintel-router": ["starintel-prepare"],
    }
    if name == "wait-server":
        wait_for(lambda: (root / "starintel-server").exists(), "server readiness")
        return
    for dependency in required.get(name, []):
        if not (root / dependency).exists():
            raise RuntimeError(name + " ran before " + dependency)
    if name == "starintel-server":
        time.sleep(0.5)  # ExecStartPost must gate dependants.
    (root / name).write_text(str(os.getpid()))
    with (root / "events").open("a") as log:
        log.write(name + "\n")
    if name not in ("starintel-prepare", "starintel-infrastructure"):
        while True:
            time.sleep(1)


def main():
    if len(sys.argv) == 4 and sys.argv[1] == "--probe":
        probe(sys.argv[2], sys.argv[3])
        return
    if pwd.getpwuid(os.getuid()).pw_name != "starintel-wiring-ci":
        raise RuntimeError("Requires dedicated disposable starintel-wiring-ci user")
    config = json.loads(Path(sys.argv[1]).read_text())
    root = Path(tempfile.mkdtemp(prefix="starintel-wiring-"))
    units = Path.home() / ".config/systemd/user"
    units.mkdir(parents=True, exist_ok=True)
    if list(units.glob("starintel*")):
        raise RuntimeError("Refusing to replace existing StarIntel units")
    script = Path(__file__).resolve()
    def command(name):
        return shlex.join([sys.executable, str(script), "--probe", name, str(root)])
    def write_unit(name, sections):
        lines = []
        for section, settings in sections.items():
            lines.append("[" + section + "]")
            for key, value in settings.items():
                for item in value if isinstance(value, list) else [value]:
                    if isinstance(item, bool):
                        item = str(item).lower()
                    lines.append(f"{key}={item}")
        (units / name).write_text("\n".join(lines) + "\n")
    names = list(config["services"])
    for name, unit in config["services"].items():
        service = unit["Service"]
        for field in ("ExecStartPost", "ExecStopPost", "Environment", "EnvironmentFile"):
            service.pop(field, None)
        service["ExecStart"] = command(name)
        service["WorkingDirectory"] = str(root)
        service["RestartSec"] = 1
        if name == "starintel-server":
            service["ExecStartPost"] = command("wait-server")
        write_unit(name + ".service", unit)
    write_unit("starintel.target", config["targets"]["starintel"])
    systemctl("daemon-reload")
    systemctl("enable", "starintel.target")
    assert (units / "default.target.wants/starintel.target").is_symlink()
    try:
        # Starting the manager's default target pulls in the enabled stack.
        systemctl("start", "default.target")
        def active(name):
            return systemctl("show", name, "--property=ActiveState", "--value") == "active"
        wait_for(lambda: all(active(n + ".service") for n in names), "automatic startup")
        # Type=simple becomes active before the probe has executed. Wait for
        # every recorded start before asserting ordering; keep all assertions.
        wait_for(lambda: (root / "events").exists()
                 and set(names).issubset((root / "events").read_text().splitlines()),
                 "all startup probes recorded")
        events = (root / "events").read_text().splitlines()
        assert events.index("starintel-prepare") < events.index("starintel-infrastructure")
        assert events.index("starintel-infrastructure") < events.index("starintel-server")
        for name in ("starintel-quasar", "starintel-actor-fixture"):
            assert events.index("starintel-server") < events.index(name)
        previous = (root / "starintel-server").read_text()
        (root / "starintel-server").unlink()
        systemctl("kill", "--signal=KILL", "starintel-server.service")
        wait_for(lambda: (root / "starintel-server").exists()
                 and (root / "starintel-server").read_text() != previous
                 and all(active(n + ".service") for n in names), "failure recovery")
        # A stopped prerequisite must be recovered with its dependants.
        systemctl("stop", "starintel-infrastructure.service")
        wait_for(lambda: all(active(n + ".service") for n in names), "prerequisite recovery")
        systemctl("stop", "starintel.target")
        wait_for(lambda: all(not active(n + ".service") for n in names), "target shutdown")
        count = len((root / "events").read_text().splitlines())
        time.sleep(2)
        assert len((root / "events").read_text().splitlines()) == count
        print("PASS: default-target startup, readiness ordering, crash/prerequisite recovery, clean stop")
    finally:
        subprocess.run(["journalctl", "--user", "--no-pager", "-n", "100"], timeout=15)
        systemctl("stop", "starintel.target")
        systemctl("disable", "starintel.target")


if __name__ == "__main__":
    main()
