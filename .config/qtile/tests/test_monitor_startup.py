#!/usr/bin/env python3
"""Regression tests for Qtile-owned monitor profile startup."""

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[3]
CONFIG = ROOT / ".config" / "qtile" / "config.py"
SOURCE = ROOT / ".config" / "qtile" / "qtile-ai.org"
AUTOSTART = ROOT / ".config" / "qtile" / "scripts" / "autostart.sh"
XINITRC = ROOT / ".xinitrc"
MONITOR_LOADER = ROOT / ".config" / "qtile" / "scripts" / "setup-monitors.sh"


class MonitorStartupTests(unittest.TestCase):
    def test_active_config_launches_selector_on_every_qtile_start(self):
        text = CONFIG.read_text(encoding="utf-8")
        self.assertIn(
            "monitor_setup = os.path.join(home, '.config/qtile/scripts/setup-monitors.sh')",
            text,
        )
        self.assertIn("subprocess.run([monitor_setup], check=False)", text)
        self.assertIn("if not globals().get('_monitor_setup_complete', False):", text)
        self.assertIn("_monitor_setup_complete = True", text)
        self.assertLess(text.index("subprocess.run([monitor_setup]"), text.index("groups = []"))
        self.assertNotIn("@hook.subscribe.startup\n", text)

    def test_literate_source_owns_the_same_startup_behavior(self):
        text = SOURCE.read_text(encoding="utf-8")
        workspace = text.index("* Workspace Configuration")
        self.assertIn("subprocess.run([monitor_setup], check=False)", text)
        self.assertIn("if not globals().get('_monitor_setup_complete', False):", text)
        self.assertLess(
            text.index("subprocess.run([monitor_setup]"),
            workspace,
        )

    def test_selector_keeps_user_hostname_profile_convention(self):
        text = MONITOR_LOADER.read_text(encoding="utf-8")
        self.assertIn('$(id -un)@$(hostname).sh', text)

    def test_session_launchers_do_not_own_monitor_startup(self):
        self.assertNotIn("setup-monitors", AUTOSTART.read_text(encoding="utf-8"))
        self.assertNotIn("setup-monitors", XINITRC.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
