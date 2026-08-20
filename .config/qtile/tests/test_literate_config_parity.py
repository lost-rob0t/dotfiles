#!/usr/bin/env python3
"""Regression tests for literate configuration source/generated parity."""

from __future__ import annotations

import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
QTILE_MAIN_ORG = ROOT / ".config" / "qtile" / "qtile-ai.org"
QTILE_MAIN_PY = ROOT / ".config" / "qtile" / "config.py"
QTILE_ORG = ROOT / ".config" / "qtile" / "qtile-openrouter.org"
QTILE_PY = ROOT / ".config" / "qtile" / "qtile_openrouter.py"
BASH_ORG = ROOT / "bash.org"
BASHRC = ROOT / ".bashrc"
BASE_NIX = ROOT / "nix" / "home-manager" / "mods" / "base.nix"
DESKTOP_NIX = ROOT / "nix" / "home-manager" / "mods" / "desktop.nix"
GIT_SYNC_SOURCE = 'source "$HOME/.config/bash/git-sync.sh"'


def _single_python_block(path: Path) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    start = lines.index("#+begin_src python") + 1
    end = lines.index("#+end_src", start)
    return "\n".join(lines[start:end]) + "\n"


class LiterateConfigParityTests(unittest.TestCase):
    def test_qtile_main_org_matches_runtime_exactly(self):
        self.assertEqual(
            _single_python_block(QTILE_MAIN_ORG),
            QTILE_MAIN_PY.read_text(encoding="utf-8"),
        )

    def test_qtile_openrouter_org_matches_runtime_exactly(self):
        self.assertEqual(
            _single_python_block(QTILE_ORG),
            QTILE_PY.read_text(encoding="utf-8"),
        )

    def test_brave_bindings_do_not_depend_on_session_path(self):
        source = QTILE_MAIN_PY.read_text(encoding="utf-8")
        self.assertIn(
            'brave_command = home + "/.nix-profile/bin/brave --new-window"',
            source,
        )
        self.assertIn(
            'Key([mod], "w", lazy.spawn(brave_command), desc="Launch Brave")',
            source,
        )
        self.assertIn(
            'Key([mod], "F1", lazy.spawn(brave_command), desc="Launch Brave")',
            source,
        )

    def test_home_manager_generates_brave_desktop_entry(self):
        source = DESKTOP_NIX.read_text(encoding="utf-8")
        self.assertIn("xdg.desktopEntries.brave-browser", source)
        self.assertIn(
            'exec = "${pkgs.brave}/bin/brave --new-window %U";',
            source,
        )

    def test_bash_org_and_bashrc_both_load_git_sync(self):
        self.assertIn(GIT_SYNC_SOURCE, BASH_ORG.read_text(encoding="utf-8"))
        self.assertIn(GIT_SYNC_SOURCE, BASHRC.read_text(encoding="utf-8"))

    def test_home_manager_installs_git_sync_command_from_shared_helper(self):
        source = BASE_NIX.read_text(encoding="utf-8")
        helper_path = "../../../.config/bash/git-sync.sh"
        self.assertIn('name = "git-sync";', source)
        self.assertIn(f"builtins.readFile {helper_path}", source)
        self.assertIn("]) ++ [ gitSync ];", source)

    def test_tangled_bashrc_is_valid_bash(self):
        completed = subprocess.run(
            ["bash", "-n", str(BASHRC)],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)


if __name__ == "__main__":
    unittest.main()
