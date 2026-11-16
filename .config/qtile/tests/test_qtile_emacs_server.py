#!/usr/bin/env python3
"""Regression tests for the dotfiles-owned Qtile Emacs server startup."""

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[3]
AUTOSTART = ROOT / ".config" / "qtile" / "scripts" / "autostart.sh"
SCRIPTS = ROOT / ".config" / "qtile" / "scripts"


class QtileEmacsServerTests(unittest.TestCase):
    def test_autostart_ensures_the_default_server_without_editor_fallback(self):
        text = AUTOSTART.read_text(encoding="utf-8")
        self.assertIn("timeout 5 emacsclient -a false --eval t", text)
        self.assertIn('setsid "$emacs_bin" --daemon', text)
        self.assertIn(".local/bin/emacs", text)
        self.assertNotIn("--daemon=qtile", text)
        self.assertNotIn("-s qtile", text)

    def test_scratchpad_scripts_use_the_default_server(self):
        for name in ("eclient.sh", "eclient-eval.sh", "org-capture.sh"):
            text = (SCRIPTS / name).read_text(encoding="utf-8")
            self.assertIn("emacsclient -a false", text)
            self.assertNotIn("-s qtile", text)


if __name__ == "__main__":
    unittest.main()
