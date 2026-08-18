#!/usr/bin/env python3
"""Tests for the Bash git-sync helper used by the Qtile reload chord."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "bash" / "git-sync.sh"


def run(*args: str, cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        args,
        cwd=cwd,
        check=False,
        text=True,
        capture_output=True,
    )


class GitSyncTests(unittest.TestCase):
    def test_fast_forwards_to_configured_upstream(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            remote = root / "remote.git"
            first = root / "first"
            second = root / "second"

            self.assertEqual(run("git", "init", "--bare", str(remote)).returncode, 0)
            self.assertEqual(run("git", "clone", str(remote), str(first)).returncode, 0)
            for repo in (first,):
                self.assertEqual(run("git", "config", "user.name", "test", cwd=repo).returncode, 0)
                self.assertEqual(run("git", "config", "user.email", "test@example.com", cwd=repo).returncode, 0)

            (first / "value").write_text("one\n", encoding="utf-8")
            self.assertEqual(run("git", "add", "value", cwd=first).returncode, 0)
            self.assertEqual(run("git", "commit", "-m", "one", cwd=first).returncode, 0)
            self.assertEqual(run("git", "push", "-u", "origin", "HEAD", cwd=first).returncode, 0)

            self.assertEqual(run("git", "clone", str(remote), str(second)).returncode, 0)
            self.assertEqual(run("git", "config", "user.name", "test", cwd=second).returncode, 0)
            self.assertEqual(run("git", "config", "user.email", "test@example.com", cwd=second).returncode, 0)
            with (second / "value").open("a", encoding="utf-8") as stream:
                stream.write("two\n")
            self.assertEqual(run("git", "add", "value", cwd=second).returncode, 0)
            self.assertEqual(run("git", "commit", "-m", "two", cwd=second).returncode, 0)
            self.assertEqual(run("git", "push", cwd=second).returncode, 0)

            command = f'source "{SCRIPT}"; git-sync "{first}"'
            synced = run("bash", "--noprofile", "--norc", "-c", command)
            self.assertEqual(synced.returncode, 0, synced.stderr)
            self.assertEqual((first / "value").read_text(encoding="utf-8"), "one\ntwo\n")

    def test_rejects_non_repository(self):
        with tempfile.TemporaryDirectory() as directory:
            command = f'source "{SCRIPT}"; git-sync "{directory}"'
            result = run("bash", "--noprofile", "--norc", "-c", command)
            self.assertEqual(result.returncode, 2)
            self.assertIn("not a git repository", result.stderr)

    def test_rejects_repository_without_upstream(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            self.assertEqual(run("git", "init", str(repo)).returncode, 0)
            command = f'source "{SCRIPT}"; git-sync "{repo}"'
            result = run("bash", "--noprofile", "--norc", "-c", command)
            self.assertEqual(result.returncode, 3)
            self.assertIn("no upstream configured", result.stderr)


if __name__ == "__main__":
    unittest.main()
