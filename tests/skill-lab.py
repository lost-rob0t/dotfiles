#!/usr/bin/env python3
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
DRIVER = ROOT / "scripts" / "skill-lab.py"


class SkillLabTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.repo = self.root / "skills"
        self.repo.mkdir()
        self._run(["git", "init", "-b", "main"], self.repo)
        self._run(["git", "config", "user.email", "test@example.invalid"], self.repo)
        self._run(["git", "config", "user.name", "Skill Lab Test"], self.repo)
        (self.repo / "AGENTS.md").write_text("test rules\n", encoding="utf-8")
        (self.repo / "skills" / "demo").mkdir(parents=True)
        (self.repo / "skills" / "demo" / "SKILL.md").write_text(
            "---\nname: demo\ndescription: demo, test\n---\n# Demo\n",
            encoding="utf-8",
        )
        scripts = self.repo / "scripts"
        scripts.mkdir()
        for name in ("validate-skills", "validate-support-scripts"):
            path = scripts / name
            path.write_text("#!/usr/bin/env bash\nexit 0\n", encoding="utf-8")
            path.chmod(0o755)
        self._run(["git", "add", "."], self.repo)
        self._run(["git", "commit", "-m", "fixture"], self.repo)
        self.env = os.environ.copy()
        self.env["XDG_CACHE_HOME"] = str(self.root / "cache")

    def tearDown(self):
        self.tmp.cleanup()

    def _run(self, argv, cwd):
        return subprocess.run(argv, cwd=cwd, check=True, text=True, capture_output=True)

    def _worker(self, body):
        path = self.root / "worker.py"
        path.write_text(
            "#!/usr/bin/env python3\n"
            "from pathlib import Path\n"
            "import sys\n"
            "args = sys.argv\n"
            "root = Path(args[args.index('--dir') + 1])\n"
            + body
            + "\nsys.stdin.read()\n",
            encoding="utf-8",
        )
        path.chmod(0o755)
        self.env["SKILL_LAB_WORKER"] = str(path)

    def _lab(self):
        return subprocess.run(
            [
                sys.executable,
                str(DRIVER),
                "run",
                "--repo", str(self.repo),
                "--skill", "demo",
            ],
            env=self.env,
            text=True,
            capture_output=True,
        )

    def test_candidate_is_isolated_and_reviewable(self):
        self._worker(
            "(root / 'skills' / 'demo' / 'SKILL.md').write_text("
            "(root / 'skills' / 'demo' / 'SKILL.md').read_text() + '\\nImproved.\\n')"
        )
        proc = self._lab()
        self.assertEqual(proc.returncode, 0, proc.stderr)
        result = json.loads(proc.stdout)
        self.assertEqual(result["status"], "ready")
        self.assertEqual(result["changed_paths"], ["skills/demo/SKILL.md"])
        self.assertTrue(Path(result["worktree"]).is_dir())
        source_status = self._run(["git", "status", "--porcelain"], self.repo).stdout
        self.assertEqual(source_status, "")

    def test_no_change_cache_has_no_deleted_worktree(self):
        self._worker("pass")
        proc = self._lab()
        self.assertEqual(proc.returncode, 0, proc.stderr)
        result = json.loads(proc.stdout)
        self.assertEqual(result["status"], "no-change")
        self.assertIsNone(result["worktree"])
        latest = json.loads(
            (Path(self.env["XDG_CACHE_HOME"]) / "skill-lab" / "latest.json").read_text()
        )
        self.assertIsNone(latest["worktree"])

    def test_scope_violation_is_not_ready(self):
        self._worker("(root / 'README.md').write_text('nope\\n')")
        proc = self._lab()
        self.assertEqual(proc.returncode, 4)
        result = json.loads(proc.stdout)
        self.assertEqual(result["status"], "scope-violation")


if __name__ == "__main__":
    unittest.main()
