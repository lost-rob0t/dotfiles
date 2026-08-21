#!/usr/bin/env python3
"""Regression coverage for Brave launch/discovery under the Qtile LightDM session."""

from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
XPROFILE = ROOT / ".xprofile"
DESKTOP_NIX = ROOT / "nix" / "home-manager" / "mods" / "desktop.nix"
QTILE_CONFIG = ROOT / ".config" / "qtile" / "config.py"


class BraveLauncherSessionTests(unittest.TestCase):
    def test_xprofile_loads_home_manager_session_and_application_data(self):
        source = XPROFILE.read_text(encoding="utf-8")
        self.assertIn("hm-session-vars.sh", source)
        self.assertIn('$HOME/.nix-profile/share', source)
        self.assertIn("XDG_DATA_DIRS", source)

    def test_desktop_home_manager_owns_xprofile(self):
        source = DESKTOP_NIX.read_text(encoding="utf-8")
        self.assertIn('home.file.".xprofile"', source)
        self.assertIn("source = ../../../.xprofile;", source)
        self.assertIn("force = true;", source)

    def test_desktop_home_manager_declares_local_brave_desktop_entry(self):
        source = DESKTOP_NIX.read_text(encoding="utf-8")
        self.assertIn("xdg.desktopEntries.brave", source)
        self.assertIn('exec = "${pkgs.brave}/bin/brave %U";', source)
        self.assertIn('categories = [ "Network" "WebBrowser" ];', source)

    def test_qtile_super_w_still_targets_brave(self):
        source = QTILE_CONFIG.read_text(encoding="utf-8")
        self.assertIn('Key([mod], "w", lazy.spawn("brave"), desc="Launch Brave")', source)


if __name__ == "__main__":
    unittest.main()
