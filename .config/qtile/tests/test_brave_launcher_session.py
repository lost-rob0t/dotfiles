#!/usr/bin/env python3
"""Regression coverage for application discovery under the Qtile LightDM session."""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
XPROFILE = ROOT / ".xprofile"
DESKTOP_NIX = ROOT / "nix" / "home-manager" / "mods" / "desktop.nix"
QTILE_CONFIG = ROOT / ".config" / "qtile" / "config.py"


class BraveLauncherSessionTests(unittest.TestCase):
    def test_xprofile_exposes_arch_and_nix_application_dirs(self):
        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            profile_dir = home / ".nix-profile" / "etc" / "profile.d"
            bin_dir = home / "bin"
            profile_dir.mkdir(parents=True)
            bin_dir.mkdir()

            (profile_dir / "hm-session-vars.sh").write_text(
                'export XDG_DATA_DIRS="/nix/hm/share:/nix/default/share"\n',
                encoding="utf-8",
            )
            systemctl = bin_dir / "systemctl"
            systemctl.write_text("#!/bin/sh\nexit 0\n", encoding="utf-8")
            systemctl.chmod(0o755)

            env = os.environ.copy()
            env["HOME"] = str(home)
            env["USER"] = "tester"
            env["PATH"] = f"{bin_dir}:{env.get('PATH', '')}"
            env.pop("XDG_DATA_HOME", None)
            env.pop("XDG_DATA_DIRS", None)

            result = subprocess.run(
                [
                    "/bin/sh",
                    "-c",
                    '. "$1"; printf "%s\\n%s\\n" "$XDG_DATA_HOME" "$XDG_DATA_DIRS"',
                    "sh",
                    str(XPROFILE),
                ],
                check=True,
                capture_output=True,
                text=True,
                env=env,
            )
            data_home, data_dirs = result.stdout.splitlines()
            dirs = data_dirs.split(":")

            self.assertEqual(data_home, str(home / ".local" / "share"))
            self.assertIn(str(home / ".nix-profile" / "share"), dirs)
            self.assertIn(str(home / ".local/state/nix/profiles/profile/share"), dirs)
            self.assertIn("/nix/var/nix/profiles/per-user/tester/profile/share", dirs)
            self.assertIn("/etc/profiles/per-user/tester/share", dirs)
            self.assertIn("/nix/hm/share", dirs)
            self.assertIn("/nix/default/share", dirs)
            self.assertIn("/usr/local/share", dirs)
            self.assertIn("/usr/share", dirs)
            self.assertEqual(len(dirs), len(set(dirs)))

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
