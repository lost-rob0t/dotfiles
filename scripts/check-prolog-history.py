#!/usr/bin/env python3
"""Reject deleted/rewritten Prolog history and untracked project KB sources."""
from __future__ import annotations

import argparse
from pathlib import Path
import subprocess


def git(root: Path, *args: str) -> bytes:
    result = subprocess.run(["git", "-C", str(root), *args], check=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30)
    return result.stdout


def sources(root: Path, revision: str) -> dict[str, tuple[str, str]]:
    result: dict[str, tuple[str, str]] = {}
    for row in git(root, "ls-tree", "-rz", "--full-tree", revision).split(b"\0"):
        if not row:
            continue
        metadata, raw_path = row.split(b"\t", 1)
        mode, kind, sha = metadata.decode("ascii").split()
        path = raw_path.decode("utf-8")
        if path.endswith(".pl"):
            if kind != "blob" or mode not in {"100644", "100755"}:
                raise ValueError(f"Prolog source is not a regular tracked blob: {path}")
            result[path] = (mode, sha)
    return result


def check(root: Path, base: str, head: str = "HEAD", *, worktree: bool = True) -> None:
    # Resolve refs to commits before building any other revision argument.
    base_sha = git(root, "rev-parse", "--verify", "--end-of-options", f"{base}^{{commit}}").decode().strip()
    head_sha = git(root, "rev-parse", "--verify", "--end-of-options", f"{head}^{{commit}}").decode().strip()
    old, new = sources(root, base_sha), sources(root, head_sha)
    errors: list[str] = []
    for path, (mode, sha) in old.items():
        if path not in new:
            errors.append(f"deleted or renamed historical Prolog: {path}")
            continue
        new_mode, new_sha = new[path]
        before = git(root, "cat-file", "blob", sha)
        after = git(root, "cat-file", "blob", new_sha)
        if mode != new_mode or not after.startswith(before):
            errors.append(f"rewritten historical Prolog: {path}; add a versioned successor instead")
    if worktree:
        tracked = {p.decode("utf-8") for p in git(root, "ls-files", "-z").split(b"\0") if p}
        for path, (_, sha) in new.items():
            file = root / path
            if file.is_symlink() or not file.is_file():
                errors.append(f"working-tree Prolog missing or symlinked: {path}")
            elif file.read_bytes() != git(root, "cat-file", "blob", sha):
                errors.append(f"uncommitted Prolog differs from {head_sha[:12]}: {path}")
        for directory in (root / ".prolog", root / ".zara"):
            if directory.is_dir():
                for file in directory.rglob("*.pl"):
                    path = file.relative_to(root).as_posix()
                    if path not in tracked or path not in new:
                        errors.append(f"KB source is not committed in Git: {path}")
    if errors:
        raise ValueError("\n".join(errors))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("base", help="known base commit, never an untrusted shell expression")
    parser.add_argument("--head", default="HEAD")
    parser.add_argument("--repo", type=Path, default=Path.cwd())
    args = parser.parse_args()
    try:
        check(args.repo.resolve(), args.base, args.head)
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        parser.exit(1, f"Prolog preservation FAILED: {error}\n")
    print("Prolog preservation PASS: prior bytes retained; KB sources committed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
