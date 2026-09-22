#!/usr/bin/env python3
"""Regression tests for the sandboxed stream Emacs launcher."""

from __future__ import annotations

import subprocess
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "stream-emacs"
INIT = Path(__file__).resolve().parents[1] / "scripts" / "stream-emacs-init.el"


class StreamEmacsScriptTests(unittest.TestCase):
    def test_script_is_executable_and_valid_bash(self):
        self.assertTrue(SCRIPT.exists())
        self.assertGreater(SCRIPT.stat().st_mode & 0o111, 0)
        completed = subprocess.run(
            ["bash", "-n", str(SCRIPT)],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(completed.returncode, 0, completed.stderr)

    def test_missing_bwrap_fails_with_a_clear_error(self):
        text = SCRIPT.read_text(encoding="utf-8")
        self.assertIn('have bwrap || die "bubblewrap (bwrap) is required', text)

    def test_sandbox_binds_only_the_project_roots_read_write(self):
        text = SCRIPT.read_text(encoding="utf-8")
        self.assertIn('--bind "$projects_dir" "$projects_dir"', text)
        self.assertIn('--bind "$starintel_dir" "$starintel_dir"', text)
        # Read-write binds are limited to the two project roots, the private
        # emacs socket directory, and the X11 socket directory.
        self.assertEqual(text.count("--bind "), 4)
        self.assertIn('--tmpfs "$HOME"', text)
        self.assertIn('--ro-bind "$doom_dir" "$doom_dir"', text)
        self.assertIn('--ro-bind "$doom_core" "$doom_core"', text)

    def test_sandbox_shares_display_and_minimal_system_trees(self):
        text = SCRIPT.read_text(encoding="utf-8")
        self.assertIn("--bind /tmp/.X11-unix /tmp/.X11-unix", text)
        self.assertIn("--ro-bind /nix/store /nix/store", text)
        self.assertIn("--dev /dev", text)
        self.assertIn("--proc /proc", text)
        self.assertIn("--ro-bind /sys /sys", text)
        self.assertIn('WAYLAND_DISPLAY', text)

    def test_emacs_runs_as_stream_server_with_streamemacs_frames(self):
        text = SCRIPT.read_text(encoding="utf-8")
        self.assertIn('"StreamEmacs"', text)
        self.assertIn("stream-emacs-init.el", text)
        init = INIT.read_text(encoding="utf-8")
        self.assertIn('(setq server-name "stream")', init)
        self.assertIn("'(name . \"StreamEmacs\")", init)
        self.assertIn("'(title . \"StreamEmacs\")", init)
        self.assertIn("server-auth-dir", init)


if __name__ == "__main__":
    unittest.main()
