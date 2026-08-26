#!/usr/bin/env python3
"""Regression tests for literate configuration source/generated parity."""

from __future__ import annotations

import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
QTILE_ORG = ROOT / ".config" / "qtile" / "qtile-openrouter.org"
QTILE_PY = ROOT / ".config" / "qtile" / "qtile_openrouter.py"
QTILE_CONFIG = ROOT / ".config" / "qtile" / "config.py"
QTILE_CONFIG_ORG = ROOT / ".config" / "qtile" / "qtile-ai.org"
BASH_ORG = ROOT / "bash.org"
BASHRC = ROOT / ".bashrc"
BASE_NIX = ROOT / "nix" / "home-manager" / "mods" / "base.nix"
GIT_SYNC_SOURCE = 'source "$HOME/.config/bash/git-sync.sh"'


def _single_python_block(path: Path) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    start = lines.index("#+begin_src python") + 1
    end = lines.index("#+end_src", start)
    return "\n".join(lines[start:end]) + "\n"


class LiterateConfigParityTests(unittest.TestCase):
    def test_qtile_openrouter_org_matches_runtime_exactly(self):
        self.assertEqual(
            _single_python_block(QTILE_ORG),
            QTILE_PY.read_text(encoding="utf-8"),
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

    def test_opencode_launcher_is_present_in_source_and_tangle(self):
        for path in (BASH_ORG, BASHRC):
            source = path.read_text(encoding="utf-8")
            self.assertIn("function opencode()", source)
            self.assertIn('terminator --title "Opencode - $topic"', source)
            self.assertIn("alias oc='opencode'", source)

    def test_opencode_launcher_passes_topic_to_terminator(self):
        completed = subprocess.run(
            [
                "bash",
                "--noprofile",
                "--norc",
                "-c",
                (
                    f"source {BASHRC} >/dev/null 2>&1; "
                    "terminator() { printf '%s\\n' \"$*\"; }; "
                    "opencode --topic='Qtile AI check' -- --help"
                ),
            ],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertIn("--title Opencode - Qtile AI check", completed.stdout)
        self.assertIn("--execute", completed.stdout)
        self.assertIn("--help", completed.stdout)

    def test_auto_mode_reconciles_with_one_second_event_loop_timer(self):
        for path in (QTILE_CONFIG_ORG, QTILE_CONFIG):
            source = path.read_text(encoding="utf-8")
            self.assertIn("AUTO_GROUP_INTERVAL = 1.0", source)
            self.assertIn("qtile.call_later(", source)
            self.assertIn("reconcile_auto_grouping", source)
            self.assertIn("@hook.subscribe.shutdown", source)


if __name__ == "__main__":
    unittest.main()
