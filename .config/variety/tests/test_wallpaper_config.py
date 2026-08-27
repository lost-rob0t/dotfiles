#!/usr/bin/env python3
"""Regression tests for the Variety wallpaper script wiring."""

from pathlib import Path
import os
import unittest


ROOT = Path(__file__).resolve().parents[3]
CONFIG = ROOT / ".config" / "variety" / "variety.conf"
SCRIPTS = ROOT / ".config" / "variety" / "scripts"


def setting(name: str) -> str:
    prefix = f"{name} = "
    for line in CONFIG.read_text(encoding="utf-8").splitlines():
        if line.startswith(prefix):
            return line.removeprefix(prefix)
    raise AssertionError(f"missing Variety setting: {name}")


class VarietyWallpaperConfigTests(unittest.TestCase):
    def test_wallpaper_scripts_point_to_existing_repo_owned_files(self):
        self.assertEqual(
            setting("set_wallpaper_script"),
            "~/.config/variety/scripts/set_wallpaper",
        )
        self.assertEqual(
            setting("get_wallpaper_script"),
            "~/.config/variety/scripts/get_wallpaper",
        )

        for name in ("set_wallpaper", "get_wallpaper"):
            script = SCRIPTS / name
            self.assertTrue(script.is_file(), script)
            self.assertTrue(os.access(script, os.X_OK), script)

    def test_set_wallpaper_supports_qtile_with_feh(self):
        source = (SCRIPTS / "set_wallpaper").read_text(encoding="utf-8")
        self.assertIn("For simple WMs, use either feh or nitrogen", source)
        self.assertIn('feh --bg-fill "$WP"', source)


if __name__ == "__main__":
    unittest.main()
