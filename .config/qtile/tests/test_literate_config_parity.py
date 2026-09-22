#!/usr/bin/env python3
"""Regression tests for literate configuration source/generated parity."""

from __future__ import annotations

import os
import subprocess
import tempfile
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

    def test_home_manager_installs_shared_sync_commands(self):
        source = BASE_NIX.read_text(encoding="utf-8")
        helper_path = "../../../.config/bash/git-sync.sh"
        self.assertIn('name = "git-sync";', source)
        self.assertIn('name = "dotfiles-sync";', source)
        self.assertIn(f"builtins.readFile {helper_path}", source)
        self.assertIn("]) ++ [ gitSync dotfilesSync ];", source)

    def test_tangled_bashrc_is_valid_bash(self):
        completed = subprocess.run(
            ["bash", "-n", str(BASHRC)],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)

    def test_opencode_launcher_is_direct_in_source_and_tangle(self):
        for path in (BASH_ORG, BASHRC):
            source = path.read_text(encoding="utf-8")
            self.assertNotIn("function opencode()", source)
            self.assertNotIn("terminator --title", source)
            self.assertIn("alias oc='opencode'", source)

    def test_opencode_launcher_executes_the_path_command_directly(self):
        with tempfile.TemporaryDirectory() as tmp:
            home = Path(tmp)
            bin_dir = home / "bin"
            bin_dir.mkdir()
            (home / ".platform").write_text("test\n", encoding="utf-8")
            executable = bin_dir / "opencode"
            executable.write_text(
                "#!/usr/bin/env bash\nprintf 'direct-opencode %s\\n' \"$*\"\n",
                encoding="utf-8",
            )
            executable.chmod(0o755)
            env = {
                **os.environ,
                "HOME": str(home),
                "PATH": f"{bin_dir}:{os.environ.get('PATH', '')}",
                "TERM": "dumb",
            }
            completed = subprocess.run(
                [
                    "bash",
                    "--noprofile",
                    "--norc",
                    "-c",
                    f"source {BASHRC} >/dev/null 2>&1; opencode --help",
                ],
                cwd=ROOT,
                check=False,
                capture_output=True,
                text=True,
                env=env,
            )
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertIn("direct-opencode --help", completed.stdout)

    def test_super_t_launches_terminator_in_source_and_tangle(self):
        for path in (QTILE_CONFIG_ORG, QTILE_CONFIG):
            source = path.read_text(encoding="utf-8")
            self.assertIn(
                'Key([mod], "t", lazy.spawn(myTerm), desc="Launch Terminator")',
                source,
            )

    def test_auto_mode_routes_new_windows_without_a_polling_timer(self):
        for path in (QTILE_CONFIG_ORG, QTILE_CONFIG):
            source = path.read_text(encoding="utf-8")
            self.assertIn("def auto_group_new_window(window):", source)
            self.assertNotIn("AUTO_GROUP_INTERVAL", source)
            self.assertNotIn("reconcile_auto_grouping", source)

    def test_application_menu_binding_is_tangled(self):
        for path in (QTILE_CONFIG_ORG, QTILE_CONFIG):
            self.assertIn(
                'Key([mod], "d", lazy.spawn("j4-dmenu-desktop")',
                path.read_text(encoding="utf-8"),
            )

    def test_qtile_ai_windows_org_matches_runtime_exactly(self):
        self.assertEqual(
            _single_python_block(ROOT / ".config" / "qtile" / "qtile-ai-windows.org"),
            (ROOT / ".config" / "qtile" / "qtile_ai_windows.py").read_text(encoding="utf-8"),
        )

    def test_topic_pool_and_runtime_are_in_source_and_tangle(self):
        for path in (QTILE_CONFIG_ORG, QTILE_CONFIG):
            source = path.read_text(encoding="utf-8")
            self.assertIn("DEFAULT_TOPICS = [", source)
            self.assertIn("topic_groups = qtile_topics.build_topic_groups(DEFAULT_TOPICS)", source)
            self.assertIn("groups.extend(topic_groups)", source)
            self.assertIn("def all_group_names():", source)
            self.assertIn("groups.append(Group(name=HOLDING_GROUP_NAME", source)
            self.assertIn("qtile_topics.expose_topic_commands()", source)
            # Static digit bindings must ignore generated topic groups.
            self.assertIn("isinstance(i, Group) and i.name in group_names:", source)
            # Streamer mode wiring.
            self.assertIn("streamer_button = qtile_streamer.streamer_button", source)
            self.assertIn("lazy.function(qtile_streamer.toggle_streamer_mode, record=True)", source)
            self.assertIn('lazy.spawn(home + "/.config/qtile/scripts/stream-emacs")', source)

    def test_stream_allowlist_is_declared_in_source_and_tangle(self):
        source = (ROOT / ".config" / "qtile" / "qtile-ai-windows.org").read_text(encoding="utf-8")
        runtime = (ROOT / ".config" / "qtile" / "qtile_ai_windows.py").read_text(encoding="utf-8")
        for text in (source, runtime):
            self.assertIn("STREAM_ALLOWLIST = {", text)
            self.assertIn("def is_stream_allowed(", text)


if __name__ == "__main__":
    unittest.main()
