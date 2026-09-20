import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("history", ROOT / "scripts/check-prolog-history.py")
HISTORY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(HISTORY)


class HistoryTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.git("init", "-q")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("config", "user.name", "Fixture")
        self.file = self.root / ".prolog/kb/past.pl"
        self.file.parent.mkdir(parents=True)
        self.file.write_text("observed(past, true).\n")
        self.commit()
        self.base = self.git("rev-parse", "HEAD").decode().strip()

    def tearDown(self):
        self.temp.cleanup()

    def git(self, *args):
        return subprocess.check_output(["git", "-C", str(self.root), *args], stderr=subprocess.DEVNULL)

    def commit(self):
        self.git("add", ".")
        self.git("commit", "-qm", "fixture")

    def test_unchanged_passes(self):
        HISTORY.check(self.root, self.base)

    def test_append_and_new_version_pass(self):
        self.file.write_text(self.file.read_text() + "supersedes(new, past).\n")
        self.file.with_name("v2.pl").write_text("observed(new, true).\n")
        self.commit()
        HISTORY.check(self.root, self.base)

    def test_delete_fails(self):
        self.file.unlink()
        self.commit()
        with self.assertRaisesRegex(ValueError, "deleted"):
            HISTORY.check(self.root, self.base)

    def test_rewrite_fails(self):
        self.file.write_text("observed(past, false).\n")
        self.commit()
        with self.assertRaisesRegex(ValueError, "rewritten"):
            HISTORY.check(self.root, self.base)

    def test_rename_fails(self):
        self.file.rename(self.file.with_name("renamed.pl"))
        self.commit()
        with self.assertRaisesRegex(ValueError, "renamed"):
            HISTORY.check(self.root, self.base)

    def test_untracked_kb_fails(self):
        self.file.with_name("lost.pl").write_text("lost(fact).\n")
        with self.assertRaisesRegex(ValueError, "not committed"):
            HISTORY.check(self.root, self.base)

    def test_ignored_kb_also_fails(self):
        (self.root / ".gitignore").write_text("lost.pl\n")
        self.file.with_name("lost.pl").write_text("lost(fact).\n")
        with self.assertRaisesRegex(ValueError, "not committed"):
            HISTORY.check(self.root, self.base)

    def test_uncommitted_change_fails(self):
        self.file.write_text("changed(fact).\n")
        with self.assertRaisesRegex(ValueError, "uncommitted"):
            HISTORY.check(self.root, self.base)

    def test_symlink_replacement_fails(self):
        self.file.unlink()
        self.file.symlink_to("elsewhere")
        self.commit()
        with self.assertRaisesRegex(ValueError, "not a regular"):
            HISTORY.check(self.root, self.base)


if __name__ == "__main__":
    unittest.main()
