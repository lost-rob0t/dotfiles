import copy
import importlib.util
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

import tomlkit

HERE = Path(__file__).resolve().parent
spec = importlib.util.spec_from_file_location("zara_development_config", HERE / "apply-zara-development-config.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
SETTINGS = {
    "tasks": {"enabled": True, "max_concurrent": 2, "max_task_steps": 20, "wall_clock_minutes": 30.0, "step_log_chars": 2000},
    "plugins": {"zara-coding": {"allowed_roots": ["/home/test/Projects", "/home/test/git/worktrees"], "prolog_rlm_checkout": "/home/test/Projects/prolog-rlm", "git": "/nix/store/test-git/bin/git", "swipl": "/nix/store/test-swi/bin/swipl"}},
}


class DevelopmentConfigTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.config = self.root / "config.toml"

    def test_creates_private_config(self):
        self.assertTrue(module.apply_config(self.config, SETTINGS))
        self.assertEqual(tomlkit.parse(self.config.read_text()).unwrap(), SETTINGS)
        self.assertEqual(self.config.stat().st_mode & 0o777, 0o600)

    def test_preserves_comments_credentials_experts_and_other_settings(self):
        self.config.write_text('# operator comment\n[provider]\napi_key = "fixture-do-not-print"\n[plugins.zara-expert]\ncustom = true\n[plugins.zara-coding]\ncustom = 7\n[tasks]\nenabled = false\nother = "retain"\n')
        module.apply_config(self.config, SETTINGS)
        text = self.config.read_text()
        result = tomlkit.parse(text)
        self.assertIn("# operator comment", text)
        self.assertEqual(result["provider"]["api_key"], "fixture-do-not-print")
        self.assertTrue(result["plugins"]["zara-expert"]["custom"])
        self.assertEqual(result["plugins"]["zara-coding"]["custom"], 7)
        self.assertEqual(result["tasks"]["other"], "retain")
        self.assertTrue(result["tasks"]["enabled"])

    def test_repeated_activation_is_byte_and_inode_stable(self):
        module.apply_config(self.config, SETTINGS)
        before = self.config.stat()
        self.assertFalse(module.apply_config(self.config, SETTINGS))
        after = self.config.stat()
        self.assertEqual((before.st_ino, before.st_mtime_ns), (after.st_ino, after.st_mtime_ns))

    def test_coding_only_does_not_create_task_settings(self):
        module.apply_config(self.config, {"plugins": SETTINGS["plugins"]})
        self.assertNotIn("tasks", tomlkit.parse(self.config.read_text()))

    def test_tasks_only_does_not_create_coding_settings(self):
        module.apply_config(self.config, {"tasks": SETTINGS["tasks"]})
        self.assertNotIn("plugins", tomlkit.parse(self.config.read_text()))

    def test_rejects_live_and_broken_symlinks(self):
        target = self.root / "target"
        for exists in (False, True):
            with self.subTest(exists=exists):
                if exists:
                    target.write_text("# unchanged\n")
                self.config.symlink_to(target)
                with self.assertRaises(ValueError):
                    module.apply_config(self.config, SETTINGS)
                self.assertTrue(self.config.is_symlink())
                self.config.unlink()
        self.assertEqual(target.read_text(), "# unchanged\n")

    def test_invalid_toml_remains_untouched(self):
        self.config.write_text("[broken\n")
        with self.assertRaises(Exception):
            module.apply_config(self.config, SETTINGS)
        self.assertEqual(self.config.read_text(), "[broken\n")

    def test_scalar_and_array_tables_remain_untouched(self):
        for text in ('tasks = "no"\n', 'plugins = []\n', '[plugins]\nzara-coding = false\n'):
            with self.subTest(text=text):
                self.config.write_text(text)
                with self.assertRaises(ValueError):
                    module.apply_config(self.config, SETTINGS)
                self.assertEqual(self.config.read_text(), text)

    def test_rejects_unknown_settings_and_secret_fields(self):
        for value in ({"provider": {"api_key": "fixture"}}, {"plugins": {"other": {}}}, {"tasks": {}}):
            with self.subTest(value=value), self.assertRaises(ValueError):
                module.apply_config(self.config, value)
        self.assertFalse(self.config.exists())
        value = copy.deepcopy(SETTINGS)
        value["plugins"]["zara-coding"]["token"] = "fixture"
        with self.assertRaises(ValueError):
            module.apply_config(self.config, value)

    def test_rejects_unbounded_roots_and_relative_executables(self):
        for field, value in (("allowed_roots", ["/"]), ("allowed_roots", []), ("allowed_roots", ["relative"]), ("allowed_roots", ["/tmp", "/tmp"]), ("git", "git"), ("swipl", "/bin/swipl\n")):
            settings = copy.deepcopy(SETTINGS)
            settings["plugins"]["zara-coding"][field] = value
            with self.subTest(field=field, value=value), self.assertRaises(ValueError):
                module.apply_config(self.config, settings)

    def test_rejects_bool_integer_and_invalid_task_limits(self):
        for field, value in (("max_concurrent", True), ("max_task_steps", 0), ("wall_clock_minutes", float("nan")), ("wall_clock_minutes", float("inf")), ("wall_clock_minutes", -1), ("enabled", "true")):
            settings = copy.deepcopy(SETTINGS)
            settings["tasks"][field] = value
            with self.subTest(field=field, value=value), self.assertRaises(ValueError):
                module.apply_config(self.config, settings)

    def test_failed_replace_retains_original_and_removes_temporary(self):
        self.config.write_text("# retain\n")
        with patch.object(module.os, "replace", side_effect=OSError("simulated")):
            with self.assertRaises(OSError):
                module.apply_config(self.config, SETTINGS)
        self.assertEqual(self.config.read_text(), "# retain\n")
        self.assertEqual(sorted(p.name for p in self.root.iterdir()), ["config.toml"])

    def test_detected_concurrent_edit_is_not_overwritten(self):
        self.config.write_text("# before\n")
        identity = module._identity
        calls = 0
        def observe(path):
            nonlocal calls
            calls += 1
            if calls == 2:
                path.write_text("# concurrent edit\n")
            return identity(path)
        with patch.object(module, "_identity", side_effect=observe):
            with self.assertRaises(RuntimeError):
                module.apply_config(self.config, SETTINGS)
        self.assertEqual(self.config.read_text(), "# concurrent edit\n")

    def test_cli_does_not_print_malformed_private_config(self):
        self.config.write_text('[provider]\napi_key = "fixture-private\n')
        settings_file = self.root / "settings.json"
        settings_file.write_text(json.dumps(SETTINGS))
        result = subprocess.run([sys.executable, str(HERE / "apply-zara-development-config.py"), str(self.config), str(settings_file)], capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertNotIn("fixture-private", result.stdout + result.stderr)
        self.assertNotIn("Traceback", result.stderr)

    def test_literate_generated_module_parity(self):
        source = (HERE.parent / "mods/zara-development.org").read_text()
        generated = (HERE.parent / "mods/zara-development.nix").read_text()
        blocks = re.findall(r"^#\+begin_src nix :tangle zara-development.nix\n(.*?)^#\+end_src$", source, re.M | re.S)
        self.assertEqual(len(blocks), 1)
        self.assertEqual(blocks[0], generated)


if __name__ == "__main__":
    unittest.main()
