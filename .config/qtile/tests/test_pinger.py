#!/usr/bin/env python3
"""Regression tests for the host aliveness pinger subsystem."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[3]
SCRIPTS = ROOT / ".config" / "qtile" / "scripts"
PINGER_ORG = SCRIPTS / "pinger.org"
PINGER_PY = SCRIPTS / "pinger.py"
HOSTS_TOML = ROOT / ".config" / "hosts.example.toml"
# Real machine-local config lives outside the repo and is not tracked.
LOCAL_HOSTS_TOML = Path.home() / ".config" / "hosts.toml"
LEGACY_HOSTS = ROOT / ".config" / "hosts"
OLD_PINGER_SH = SCRIPTS / "pinger.sh"

sys.path.insert(0, str(SCRIPTS))
import pinger  # noqa: E402  (after sys.path setup)


def _single_python_block(path: Path) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    start = lines.index("#+begin_src python") + 1
    end = lines.index("#+end_src", start)
    return "\n".join(lines[start:end]) + "\n"


class PingerLiterateParityTests(unittest.TestCase):
    def test_pinger_org_tangles_to_runtime_exactly(self):
        self.assertEqual(
            _single_python_block(PINGER_ORG),
            PINGER_PY.read_text(encoding="utf-8"),
        )

    def test_legacy_plain_hosts_list_is_gone_from_repo(self):
        self.assertFalse(LEGACY_HOSTS.exists())

    def test_old_shell_pinger_is_gone(self):
        self.assertFalse(OLD_PINGER_SH.exists())

    def test_tracked_template_exists(self):
        self.assertTrue(HOSTS_TOML.is_file())


class HostsTomlTests(unittest.TestCase):
    def test_template_loads_via_pinger(self):
        interval, hosts = pinger.load_hosts(HOSTS_TOML)
        self.assertGreater(interval, 0)
        self.assertGreaterEqual(len(hosts), 1)
        for host in hosts:
            self.assertTrue(host.name)
            self.assertTrue(host.address)
            self.assertIn(host.method, ("ping", "ssh"))

    def test_template_has_ping_and_ssh_examples(self):
        _, hosts = pinger.load_hosts(HOSTS_TOML)
        methods = {h.method for h in hosts}
        self.assertIn("ping", methods)
        self.assertIn("ssh", methods)

    def test_local_machine_config_loads_if_present(self):
        # The real config is machine-local and may not exist on CI/other hosts.
        if not LOCAL_HOSTS_TOML.is_file():
            self.skipTest(f"no machine-local config at {LOCAL_HOSTS_TOML}")
        interval, hosts = pinger.load_hosts(LOCAL_HOSTS_TOML)
        self.assertGreater(interval, 0)
        self.assertGreaterEqual(len(hosts), 1)


class HostParsingTests(unittest.TestCase):
    def test_invalid_method_is_rejected(self):
        import tempfile, tomllib

        toml_text = """
interval_seconds = 5
[[hosts]]
name = "x"
address = "1.2.3.4"
method = "carrier-pigeon"
"""
        with tempfile.NamedTemporaryFile("w", suffix=".toml", delete=False) as fh:
            fh.write(toml_text)
            path = Path(fh.name)
        try:
            with self.assertRaises(ValueError):
                pinger.load_hosts(path)
        finally:
            path.unlink()

    def test_missing_name_is_rejected(self):
        import tempfile

        toml_text = """
[[hosts]]
address = "1.2.3.4"
"""
        with tempfile.NamedTemporaryFile("w", suffix=".toml", delete=False) as fh:
            fh.write(toml_text)
            path = Path(fh.name)
        try:
            with self.assertRaises(ValueError):
                pinger.load_hosts(path)
        finally:
            path.unlink()

    def test_ssh_host_picks_up_optional_fields(self):
        import tempfile

        toml_text = """
[[hosts]]
name = "lab"
address = "lab.example.com"
method = "ssh"
user = "deploy"
port = 2222
key = "/tmp/key"
timeout = 9
"""
        with tempfile.NamedTemporaryFile("w", suffix=".toml", delete=False) as fh:
            fh.write(toml_text)
            path = Path(fh.name)
        try:
            _, hosts = pinger.load_hosts(path)
        finally:
            path.unlink()
        self.assertEqual(len(hosts), 1)
        h = hosts[0]
        self.assertEqual(h.method, "ssh")
        self.assertEqual(h.user, "deploy")
        self.assertEqual(h.port, 2222)
        self.assertEqual(h.key, "/tmp/key")
        self.assertEqual(h.timeout, 9)

    def test_ssh_command_includes_user_port_and_key(self):
        host = pinger.Host(
            name="lab",
            address="lab.example.com",
            method="ssh",
            user="deploy",
            port=2222,
            key="/tmp/key",
            timeout=5,
        )
        cmd = pinger.ssh_command(host)
        self.assertIn("ssh", cmd)
        self.assertIn("-i", cmd)
        self.assertIn("/tmp/key", cmd)
        self.assertIn("-p", cmd)
        self.assertIn("2222", cmd)
        self.assertIn("deploy@lab.example.com", cmd)
        self.assertIn("true", cmd)
        # Must be non-interactive.
        self.assertIn("BatchMode=yes", cmd)
        # ConnectTimeout must be a -o option, not a bare flag.
        self.assertIn(f"ConnectTimeout={host.timeout}", cmd)

    def test_ssh_command_without_key_omits_i(self):
        host = pinger.Host(name="h", address="h", method="ssh", user="u")
        cmd = pinger.ssh_command(host)
        self.assertNotIn("-i", cmd)
        self.assertIn("u@h", cmd)

    def test_ping_command_uses_one_packet_and_timeout(self):
        host = pinger.Host(name="h", address="1.2.3.4", method="ping", timeout=7)
        cmd = pinger.ping_command(host)
        self.assertEqual(cmd[0], "ping")
        self.assertIn("-c", cmd)
        self.assertIn("1", cmd)
        self.assertIn("-W", cmd)
        self.assertIn("7", cmd)
        self.assertIn("1.2.3.4", cmd)

    def test_load_hosts_accepts_host_alias(self):
        import tempfile

        toml_text = """
[[hosts]]
name = "x"
host = "1.2.3.4"
"""
        with tempfile.NamedTemporaryFile("w", suffix=".toml", delete=False) as fh:
            fh.write(toml_text)
            path = Path(fh.name)
        try:
            _, hosts = pinger.load_hosts(path)
        finally:
            path.unlink()
        self.assertEqual(hosts[0].address, "1.2.3.4")

    def test_load_hosts_accepts_hostname_alias(self):
        import tempfile

        toml_text = """
[[hosts]]
name = "x"
hostname = "5.6.7.8"
"""
        with tempfile.NamedTemporaryFile("w", suffix=".toml", delete=False) as fh:
            fh.write(toml_text)
            path = Path(fh.name)
        try:
            _, hosts = pinger.load_hosts(path)
        finally:
            path.unlink()
        self.assertEqual(hosts[0].address, "5.6.7.8")


class NotificationFormatTests(unittest.TestCase):
    def test_down_message_includes_name_and_ip(self):
        import io
        from contextlib import redirect_stderr

        host = pinger.Host(name="Proxmox", address="10.60.60.248", method="ssh")
        # Force no notifier so notify() falls back to printing to stderr,
        # which lets us assert the exact message format.
        buf = io.StringIO()
        with redirect_stderr(buf), mock.patch.object(pinger, "_which", return_value=None):
            pinger.notify(host, down=True)
        out = buf.getvalue()
        self.assertIn("Proxmox", out)
        self.assertIn("10.60.60.248", out)
        self.assertIn("DOWN", out)

    def test_up_message_includes_name_and_ip(self):
        import io
        from contextlib import redirect_stderr

        host = pinger.Host(name="Proxmox", address="10.60.60.248", method="ssh")
        buf = io.StringIO()
        with redirect_stderr(buf), mock.patch.object(pinger, "_which", return_value=None):
            pinger.notify(host, down=False)
        out = buf.getvalue()
        self.assertIn("Proxmox", out)
        self.assertIn("10.60.60.248", out)
        self.assertIn("back up", out)


if __name__ == "__main__":
    unittest.main()
