"""Host-side structural tests for the Termux Doom desktop-reuse profile.

No Doom installation or Android SDK required.  These tests assert the
invariants documented in android/doom/config.org and android/doom/README.org:

- init.el mirrors the desktop module selection minus Android-hostile modules;
- packages.el reuses the desktop package list instead of redeclaring it;
- config.el loads the desktop config read-only and applies Android overrides
  strictly after that load.
"""
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DOOM = ROOT / "android" / "doom"

# Modules deliberately not carried over to Android (native builds).
ANDROID_DROPPED_MODULES = {"vterm", "pdf"}
# Flags deliberately dropped from desktop module selections.
ANDROID_DROPPED_FLAGS = {"+tree-sitter"}


def strip_inline_comment(line: str) -> str:
    cut = line.find(";")
    return line[:cut] if cut >= 0 else line


def enabled_module_bases(text: str) -> set:
    """Collect base module names enabled in a doom! block."""
    bases = set()
    for raw in text.splitlines():
        line = strip_inline_comment(raw).strip()
        if not line or line.startswith(":"):
            continue
        tokens = line.strip("()").split()
        if tokens and re.fullmatch(r"[a-z][a-z0-9-]*", tokens[0]):
            bases.add(tokens[0])
    return bases


def enabled_flags(text: str) -> set:
    return set(re.findall(r"\+[a-z-]+",
                          "\n".join(strip_inline_comment(l)
                                    for l in text.splitlines())))


class TestTermuxDoomDesktopReuse(unittest.TestCase):
    def setUp(self):
        self.init = (DOOM / "init.el").read_text()
        self.packages = (DOOM / "packages.el").read_text()
        self.config = (DOOM / "config.el").read_text()
        self.desktop_init = (ROOT / ".doom.d" / "init.el").read_text()

    # -- init.el: reuse with documented deviations --------------------
    def test_init_mirrors_desktop_modules(self):
        termux = enabled_module_bases(self.init)
        desktop = enabled_module_bases(self.desktop_init)
        self.assertTrue({"company", "ivy", "magit"} <= termux,
                        "termux init.el must reuse the desktop completion/git stack")
        self.assertTrue({"org", "eshell", "tty"} <= termux)
        for module in termux:
            self.assertIn(module, desktop,
                          f"module {module} is not enabled on the desktop")

    def test_init_drops_android_hostile_modules(self):
        termux = enabled_module_bases(self.init)
        for module in ANDROID_DROPPED_MODULES:
            self.assertNotIn(module, termux,
                             f"{module} must not be enabled on Android")
            self.assertIn(module, enabled_module_bases(self.desktop_init))

    def test_init_drops_tree_sitter_flags(self):
        flags = enabled_flags(self.init)
        dropped = ANDROID_DROPPED_FLAGS & flags
        self.assertFalse(dropped,
                         f"Android must not build tree-sitter grammars: {dropped}")

    # -- packages.el: desktop package list reuse ----------------------
    def test_packages_reuse_desktop_list(self):
        self.assertIn("star/termux--desktop-doomdir", self.packages)
        self.assertIn('load (expand-file-name "packages.el"', self.packages)
        self.assertIn('"~/.dotfiles/.doom.d"', self.packages)
        self.assertIn('"~/dotfiles/.doom.d"', self.packages)

    def test_packages_do_not_redeclare_desktop(self):
        declared = re.findall(r"^\(package! ([a-z0-9-]+)", self.packages, re.M)
        self.assertEqual(
            declared, [],
            "Termux profile must not keep a parallel package list: %s" % declared)

    # -- config.el: load-then-override ordering -----------------------
    def test_config_loads_desktop_before_overrides(self):
        load_call = self.config.find("(star/termux-load-desktop-config)")
        overrides = self.config.find(";;; Android overrides")
        browse = self.config.find("star/termux-browse-url")
        self.assertGreater(load_call, 0, "desktop config loader must be invoked")
        self.assertGreater(overrides, load_call,
                           "Android overrides must come after the desktop load")
        self.assertGreater(browse, overrides)

    def test_config_redirects_desktop_paths(self):
        for marker in ("nsa/lisp-directory",
                       "org-configs-list",
                       "eshell-aliases-file",
                       "org-roam-directory",
                       "alert-default-style",
                       "termux-open-url"):
            self.assertIn(marker, self.config)

    def test_config_resolves_desktop_doomdir(self):
        self.assertIn("STAR_DESKTOP_DOOMDIR", self.config)
        self.assertIn('expand-file-name "../../.doom.d" doom-private-dir',
                      self.config)

    def test_config_contains_failure_containment(self):
        self.assertIn("condition-case", self.config)
        self.assertIn("desktop config failed to load", self.config)


if __name__ == "__main__":
    unittest.main()
