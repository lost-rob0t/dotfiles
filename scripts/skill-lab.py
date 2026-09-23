#!/usr/bin/env python3
"""Isolated SkillGen-style candidate generator for the canonical skills repo."""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time
from typing import Sequence


class Error(RuntimeError):
    pass


def sh(argv: Sequence[str], cwd: Path | None = None, stdin: str | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        list(argv),
        cwd=str(cwd) if cwd else None,
        input=stdin,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


def git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    proc = sh(["git", "-C", str(repo), *args])
    if check and proc.returncode:
        raise Error((proc.stderr or proc.stdout).strip())
    return proc


def repo_path(explicit: str | None) -> Path:
    choices = (
        explicit,
        os.getenv("SKILL_LAB_REPO"),
        os.getenv("OPENCODE_GLOBAL_SKILLS_CHECKOUT"),
        str(Path.home() / "Documents" / "AI" / "skills"),
        str(Path.home() / "skills"),
    )
    for raw in choices:
        if not raw:
            continue
        path = Path(raw).expanduser().resolve()
        if (path / ".git").exists():
            top = Path(git(path, "rev-parse", "--show-toplevel").stdout.strip()).resolve()
            if top == path and (path / "AGENTS.md").is_file() and (path / "skills").is_dir():
                return path
    raise Error("skills checkout not found; set SKILL_LAB_REPO or OPENCODE_GLOBAL_SKILLS_CHECKOUT")


def names(repo: Path) -> list[str]:
    return sorted(path.parent.name for path in (repo / "skills").glob("*/SKILL.md"))


def choose(repo: Path, requested: str) -> str:
    available = names(repo)
    if requested != "auto":
        if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", requested) or requested not in available:
            raise Error(f"unknown skill: {requested}")
        return requested

    def age(name: str) -> tuple[int, str]:
        proc = git(repo, "log", "-1", "--format=%ct", "--", f"skills/{name}", check=False)
        raw = proc.stdout.strip()
        return (int(raw) if raw.isdigit() else 0, name)

    if not available:
        raise Error("no skills found")
    return min(available, key=age)


def cache() -> Path:
    return Path(os.getenv("XDG_CACHE_HOME", Path.home() / ".cache")) / "skill-lab"


def add_worktree(repo: Path, skill: str) -> tuple[str, Path]:
    stamp = time.strftime("%Y%m%d-%H%M%S")
    branch = f"lab/skill-{skill}-{stamp}"
    root = cache() / "worktrees"
    root.mkdir(parents=True, exist_ok=True)
    worktree = root / f"skill-{skill}-{stamp}"
    git(repo, "worktree", "add", "-b", branch, str(worktree), "HEAD")
    return branch, worktree


def changed(worktree: Path) -> list[str]:
    proc = git(worktree, "status", "--porcelain=v1", "--untracked-files=all")
    out = []
    for line in proc.stdout.splitlines():
        if len(line) < 4:
            continue
        path = line[3:].split(" -> ", 1)[-1]
        out.append(path)
    return sorted(set(out))


def prompt(skill: str, goal: str | None) -> str:
    operator = (
        f"Operator goal: {goal.strip()}"
        if goal and goal.strip()
        else "No operator goal: change nothing unless you find a concrete defect or omission."
    )
    return f"""You are the mutation arm of a small SkillGen/SkillOpt-style lab.
Target only: skills/{skill}/

Read AGENTS.md, the target skill, support files, relevant tests, and recent Git
history. Follow the installed skill-edit and skill-portability skills when
available. Make exactly one focused, evidence-based improvement. Prefer no
change over speculative churn.

Rules:
- Modify only skills/{skill}/.
- Do not touch catalog/CI/README/other skills/installed copies.
- Do not commit, branch, push, open PRs, or alter remotes; the harness owns Git.
- Preserve provenance. Never add credentials or private data.
- Keep SKILL.md concise; put repeatable deterministic mechanics in scripts.

{operator}
"""


def worker(worktree: Path, skill: str, model: str, goal: str | None) -> dict[str, object]:
    binary = os.getenv("SKILL_LAB_WORKER", "opencode-worker")
    if not shutil.which(binary) and not Path(binary).exists():
        raise Error(f"worker not found: {binary}")
    proc = sh(
        [
            binary,
            "--model", model,
            "--role", "skill-lab-improver",
            "--mode", "isolated-mutate",
            "--dir", str(worktree),
        ],
        stdin=prompt(skill, goal),
    )
    return {
        "ok": proc.returncode == 0,
        "returncode": proc.returncode,
        "stdout_tail": proc.stdout[-2000:],
        "stderr_tail": proc.stderr[-2000:],
    }


def check(name: str, argv: Sequence[str], cwd: Path) -> dict[str, object]:
    proc = sh(argv, cwd)
    return {
        "name": name,
        "ok": proc.returncode == 0,
        "returncode": proc.returncode,
        "output_tail": (proc.stdout + proc.stderr)[-2000:],
    }


def verify(worktree: Path, skill: str, full: bool) -> list[dict[str, object]]:
    commands: list[tuple[str, list[str]]] = [
        ("validate-skill", ["bash", "scripts/validate-skills", f"skills/{skill}"]),
        ("validate-support-scripts", ["bash", "scripts/validate-support-scripts"]),
        ("git-diff-check", ["git", "diff", "--check"]),
    ]
    focused = worktree / "tests" / f"test_{skill.replace('-', '_')}.py"
    if focused.is_file():
        commands.append(("focused-test", [sys.executable, str(focused.relative_to(worktree))]))
    if full:
        commands.append(("nix-flake-check", ["nix", "flake", "check", "--no-build", "--show-trace"]))
    return [check(name, argv, worktree) for name, argv in commands]


def save(result: dict[str, object]) -> None:
    root = cache()
    root.mkdir(parents=True, exist_ok=True)
    (root / "latest.json").write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")


def cleanup(repo: Path, worktree: Path) -> None:
    worktree = worktree.expanduser().resolve()
    listing = git(repo, "worktree", "list", "--porcelain").stdout.splitlines()
    known = {
        Path(line[9:]).resolve()
        for line in listing
        if line.startswith("worktree ")
    }
    if worktree not in known:
        raise Error(f"not a worktree of {repo}: {worktree}")
    branch = git(worktree, "branch", "--show-current", check=False).stdout.strip()
    if branch and not branch.startswith("lab/"):
        raise Error(f"refusing to remove non-lab branch: {branch}")
    git(repo, "worktree", "remove", "--force", str(worktree))
    if branch:
        git(repo, "branch", "-D", branch)


def run_candidate(args: argparse.Namespace) -> int:
    repo = repo_path(args.repo)
    skill = choose(repo, args.skill)
    if args.dry_run:
        print(json.dumps({"status": "dry-run", "repo": str(repo), "skill": skill, "model": args.model}))
        return 0

    branch, worktree = add_worktree(repo, skill)
    result: dict[str, object] = {
        "status": "started",
        "repo": str(repo),
        "skill": skill,
        "model": args.model,
        "branch": branch,
        "worktree": str(worktree),
        "source_head": git(repo, "rev-parse", "HEAD").stdout.strip(),
    }
    try:
        result["worker"] = worker(worktree, skill, args.model, args.goal)
        paths = changed(worktree)
        result["changed_paths"] = paths

        if not result["worker"]["ok"]:  # type: ignore[index]
            result["status"] = "worker-failed"
            save(result)
            print(json.dumps(result))
            return 3

        if not paths:
            result["status"] = "no-change"
            save(result)
            cleanup(repo, worktree)
            result["branch"] = None
            result["worktree"] = None
            print(json.dumps(result))
            return 0

        prefix = f"skills/{skill}/"
        if any(not path.startswith(prefix) for path in paths):
            result["status"] = "scope-violation"
            save(result)
            print(json.dumps(result))
            return 4

        checks = verify(worktree, skill, args.full)
        result["checks"] = checks
        result["status"] = "ready" if all(item["ok"] for item in checks) else "failed-validation"
        save(result)
        print(json.dumps(result))
        return 0 if result["status"] == "ready" else 5
    except Exception:
        result["status"] = "harness-error"
        save(result)
        raise


def main(argv: Sequence[str] | None = None) -> int:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="command", required=True)

    runp = sub.add_parser("run")
    runp.add_argument("--repo")
    runp.add_argument("--skill", default="auto")
    runp.add_argument("--model", default="astra-medium")
    runp.add_argument("--goal")
    runp.add_argument("--full", action="store_true")
    runp.add_argument("--dry-run", action="store_true")

    cleanp = sub.add_parser("cleanup")
    cleanp.add_argument("--repo")
    cleanp.add_argument("--worktree", required=True)

    sub.add_parser("latest")
    args = parser.parse_args(argv)

    try:
        if args.command == "run":
            return run_candidate(args)
        if args.command == "cleanup":
            repo = repo_path(args.repo)
            cleanup(repo, Path(args.worktree))
            print(json.dumps({"status": "cleaned", "worktree": args.worktree}))
            return 0
        path = cache() / "latest.json"
        if not path.is_file():
            raise Error("no skill-lab result recorded")
        sys.stdout.write(path.read_text(encoding="utf-8"))
        return 0
    except Error as exc:
        print(f"skill-lab: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
