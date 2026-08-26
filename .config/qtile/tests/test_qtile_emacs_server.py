#!/usr/bin/env python3
"""Regression tests for the dotfiles-owned Qtile Emacs server startup."""

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[3]
AUTOSTART = ROOT / ".config" / "qtile" / "scripts" / "autostart.sh"
SCRIPTS = ROOT / ".config" / "qtile" / "scripts"


class QtileEmacsServerTests(unittest.TestCase):
    def test_autostart_ensures_the_named_server_without_editor_fallback(self):
        text = AUTOSTART.read_text(encoding="utf-8")
        self.assertIn("emacsclient -s qtile -a false --eval t", text)
        self.assertIn("setsid emacs --daemon=qtile", text)

    def test_qtile_scratchpad_scripts_use_the_named_server(self):
        for name in ("eclient.sh", "eclient-eval.sh", "org-capture.sh"):
            text = (SCRIPTS / name).read_text(encoding="utf-8")
            self.assertIn("emacsclient -s qtile -a false", text)


if __name__ == "__main__":
    unittest.main()
