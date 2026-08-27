#!/usr/bin/env python3
"""Regression tests for the dotfiles-owned Qtile Emacs server startup."""

from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[3]
AUTOSTART = ROOT / ".config" / "qtile" / "scripts" / "autostart.sh"


class QtileEmacsServerTests(unittest.TestCase):
    def test_autostart_ensures_the_named_server_without_editor_fallback(self):
        text = AUTOSTART.read_text(encoding="utf-8")
        self.assertIn("emacsclient -s qtile -a false --eval t", text)
        self.assertIn("setsid emacs --daemon=qtile", text)


if __name__ == "__main__":
    unittest.main()
