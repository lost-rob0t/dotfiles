#!/usr/bin/env python3
"""Five-worker Zara plugin feature lab with Prolog-RLM symbolic admission."""
from __future__ import annotations

import argparse
import json
import os
import shutil
import signal
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any


class LabError(RuntimeError):
    pass


@dataclass(frozen=True)
class Paths:
    dotfiles: Path
    prolog_rlm: Path
    zara_plugins: Path
    state: Path


def expand(value: str) -> Path:
    return Path(value).expanduser().resolve()


def paths() -> Paths:
    state_home = expand(os.environ.get("XDG_STATE_HOME", "~/.local/state"))
    return Paths(
        dotfiles=expand(os.environ.get("ZARA_LAB_DOTFILES_ROOT", "~/.dotfiles")),
        prolog_rlm=expand(os.environ.get("ZARA_LAB_PROLOG_RLM_ROOT", "~/Documents/Projects/prolog-rlm")),
        zara_plugins=expand(os.environ.get("ZARA_LAB_ZARA_PLUGINS_ROOT", "~/Documents/Projects/zara-plugins")),
        state=state_home / "zara" / "feature-lab",
    )


def run(argv: list[str], *, cwd: Path | None = None, input_text: str | None = None,
        check: bool = True) -> subprocess.CompletedProcess[str]:
    proc = subprocess.run(
        argv,
        cwd=str(cwd) if cwd else None,
        input=input_text,
        text=True,
        capture_output=True,
    )
    if check and proc.returncode != 0:
        detail = proc.stderr.strip() or proc.stdout.strip() or f"exit {proc.returncode}"
        raise LabError(f"{argv[0]} failed: {detail}")
    return proc


def git(repo: Path, *argv: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    return run(["git", "-C", str(repo), *argv], check=check)


def require_executable(name: str) -> str:
    path = shutil.which(name)
    if not path:
        raise LabError(f"required executable is missing: {name}")
    return path


def load_specs(root: Path) -> list[dict[str, Any]]:
    path = root / ".zara/labs/zara-feature-lab/features.json"
    try:
        document = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        raise LabError(f"cannot read feature lab spec: {exc}") from exc
    workers = document.get("workers")
    if document.get("schema") != "zara-feature-lab.v1" or document.get("worker_count") != 5:
        raise LabError("feature lab schema/worker_count mismatch")
    if not isinstance(workers, list) or len(workers) != 5:
        raise LabError("feature lab must define exactly five workers")
    ids = [item.get("id") for item in workers]
    if len(set(ids)) != 5 or any(not isinstance(item, str) or not item for item in ids):
        raise LabError("feature worker ids must be five unique non-empty strings")
    return workers


def state_file(p: Paths) -> Path:
    return p.state / "state.json"


def load_state(p: Paths) -> dict[str, Any]:
    try:
        return json.loads(state_file(p).read_text())
    except FileNotFoundError:
        return {"schema": "zara-feature-lab.state.v1", "workers": {}}
    except json.JSONDecodeError as exc:
        raise LabError(f"invalid state file: {exc}") from exc


def save_state(p: Paths, state: dict[str, Any]) -> None:
    p.state.mkdir(parents=True, exist_ok=True)
    temp = state_file(p).with_suffix(".tmp")
    temp.write_text(json.dumps(state, indent=2, sort_keys=True) + "\n")
    os.chmod(temp, 0o600)
    temp.replace(state_file(p))


def alive(pid: int | None) -> bool:
    if not isinstance(pid, int) or pid <= 0:
        return False
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def validate_repo(path: Path, label: str) -> None:
    if not path.is_dir():
        raise LabError(f"{label} checkout does not exist: {path}")
    git(path, "rev-parse", "--show-toplevel")


def branch_exists(repo: Path, branch: str) -> bool:
    return git(repo, "show-ref", "--verify", "--quiet", f"refs/heads/{branch}", check=False).returncode == 0


def ensure_worktree(repo: Path, worktree: Path, branch: str, base: str) -> None:
    if worktree.exists():
        top = git(worktree, "rev-parse", "--show-toplevel").stdout.strip()
        if Path(top).resolve() != worktree.resolve():
            raise LabError(f"unexpected existing worktree root: {worktree}")
        return
    worktree.parent.mkdir(parents=True, exist_ok=True)
    if branch_exists(repo, branch):
        run(["git", "-C", str(repo), "worktree", "add", str(worktree), branch])
    else:
        run(["git", "-C", str(repo), "worktree", "add", "-b", branch, str(worktree), base])


def admission(p: Paths, worktree: Path, spec: dict[str, Any]) -> dict[str, Any]:
    gate = p.dotfiles / "scripts/zara-feature-lab-runtime.pl"
    request = {
        "prolog_rlm_root": str(p.prolog_rlm),
        "expert_file": str(worktree / spec["expert_file"]),
        "expert_module": spec["expert_module"],
    }
    proc = run(
        ["swipl", "-q", "-s", str(gate)],
        input_text=json.dumps(request) + "\n",
        check=False,
    )
    try:
        result = json.loads(proc.stdout)
    except json.JSONDecodeError as exc:
        raise LabError(f"{spec['id']}: symbolic admission returned invalid JSON") from exc
    if proc.returncode != 0 or result.get("ok") is not True:
        raise LabError(f"{spec['id']}: Prolog-RLM symbolic admission failed: {result}")
    if (
        result.get("runtime") != "prolog-rlm"
        or result.get("runtime_ready") is not True
        or result.get("provider_policy") != "disabled"
        or result.get("max_model_calls") != 0
        or result.get("model_calls") != 0
        or result.get("reasoning_mode_contract") not in {"pending-upstream", "selector-v1"}
        or result.get("effective_reasoning_mode") not in {"zero-model-expert-first", "symbolic"}
        or (
            result.get("reasoning_mode_contract") == "selector-v1"
            and result.get("effective_reasoning_mode") != "symbolic"
        )
    ):
        raise LabError(f"{spec['id']}: symbolic admission contract mismatch")
    return result


def task_prompt(p: Paths, worktree: Path, spec: dict[str, Any], admitted: dict[str, Any]) -> str:
    overlay = f".zara/labs/zara-feature-lab/features/{spec['id']}/overlay"
    return f"""You are Zara Feature Lab worker {spec['id']} ({spec['role']}).

Goal:
{spec['goal']}

Mandatory architecture:
- Prolog-RLM admission already passed: {json.dumps(admitted, sort_keys=True)}.
- Start symbolic-first. The canonical expert is {spec['expert_file']}; preserve its zero-model/provider-disabled policy.
- Do not invent a second Zara expert registry, permission system, scheduler, provider runtime, conversation store, or Prolog-RLM reasoning-mode contract.
- Build the promotable plugin delta under {overlay}/. That directory mirrors paths relative to {spec['promotion_path']} in zara-plugins.
- Canonical expert-brain changes belong under .zara/experts/ in Dotfiles, not in the plugin overlay.
- The downstream target is {spec['target_plugin']} at {spec['promotion_path']}.
- If {p.zara_plugins} is readable, inspect the existing target plugin for compatibility but never mutate that checkout from this worker.
- Use typed tools and fresh postcondition evidence for effects. No raw shell-string execution paths.
- Secrets/API keys/tokens may come only from environment variables, OS wallet/keyring, or Emacs auth-source. Never write secret values to Git, Nix store paths, generated configs, logs, or prompts.
- Use the repository's prolog-verification workflow for every file-changing task. Record exact tests and finish only with prolog-verify check passing.
- Commit the intended candidate on this worker branch before the final verification pass. Leave feature code and overlay files clean; verifier runtime state may remain local.
- Preserve existing Org/literate source ownership; edit the canonical source and generated file together when required.
- Do not merge, force-push, reset, clean, or modify master.

Worktree: {worktree}
Promotion is operator-controlled by: zara-feature-lab promote {spec['id']}
"""


def start_worker(p: Paths, spec: dict[str, Any], state: dict[str, Any]) -> dict[str, Any]:
    worker_id = spec["id"]
    prior = state.get("workers", {}).get(worker_id, {})
    if alive(prior.get("pid")):
        return prior

    branch = f"zara-lab/{worker_id}"
    worktree = p.state / "worktrees" / worker_id
    base = git(p.dotfiles, "rev-parse", "HEAD").stdout.strip()
    ensure_worktree(p.dotfiles, worktree, branch, base)
    overlay = worktree / ".zara/labs/zara-feature-lab/features" / worker_id / "overlay"
    overlay.mkdir(parents=True, exist_ok=True)

    admitted = admission(p, worktree, spec)
    run_dir = p.state / "runs" / time.strftime("%Y%m%dT%H%M%S")
    run_dir.mkdir(parents=True, exist_ok=True)
    stdout_path = run_dir / f"{worker_id}.stdout.log"
    stderr_path = run_dir / f"{worker_id}.stderr.log"

    argv = [
        "opencode-worker",
        "--format", "json",
        "--max-retries", "0",
        "--mode", "isolated-mutate",
        "--role", spec["role"],
        "--dir", str(worktree),
    ]
    model = os.environ.get("ZARA_LAB_WORKER_MODEL", "").strip()
    if model:
        argv[1:1] = ["--model", model]

    prompt = task_prompt(p, worktree, spec, admitted)
    with stdout_path.open("w") as out, stderr_path.open("w") as err:
        proc = subprocess.Popen(
            argv,
            stdin=subprocess.PIPE,
            stdout=out,
            stderr=err,
            text=True,
            start_new_session=True,
        )
        assert proc.stdin is not None
        proc.stdin.write(prompt)
        proc.stdin.close()

    return {
        "id": worker_id,
        "pid": proc.pid,
        "branch": branch,
        "worktree": str(worktree),
        "stdout": str(stdout_path),
        "stderr": str(stderr_path),
        "started_at": int(time.time()),
        "admission": admitted,
    }


def cmd_start(p: Paths, _: argparse.Namespace) -> dict[str, Any]:
    for name in ("git", "swipl", "opencode-worker", "prolog-verify"):
        require_executable(name)
    validate_repo(p.dotfiles, "Dotfiles")
    if not (p.prolog_rlm / "prolog/rlm.pl").is_file():
        raise LabError(f"Prolog-RLM checkout is missing prolog/rlm.pl: {p.prolog_rlm}")
    specs = load_specs(p.dotfiles)
    state = load_state(p)
    state["schema"] = "zara-feature-lab.state.v1"
    state["dotfiles_head"] = git(p.dotfiles, "rev-parse", "HEAD").stdout.strip()
    state["workers"] = state.get("workers", {})
    for spec in specs:
        state["workers"][spec["id"]] = start_worker(p, spec, state)
        save_state(p, state)
    selected = [state["workers"][spec["id"]] for spec in specs]
    return {
        "ok": True,
        "worker_count": 5,
        "workers": [
            {"id": item["id"], "pid": item["pid"], "branch": item["branch"], "worktree": item["worktree"]}
            for item in selected
        ],
    }


def worker_rows(p: Paths) -> list[dict[str, Any]]:
    state = load_state(p)
    rows = []
    for worker_id, item in sorted(state.get("workers", {}).items()):
        row = dict(item)
        row["id"] = worker_id
        row["running"] = alive(item.get("pid"))
        rows.append(row)
    return rows


def cmd_status(p: Paths, _: argparse.Namespace) -> dict[str, Any]:
    specs = load_specs(p.dotfiles)
    configured = [item["id"] for item in specs]
    rows = [row for row in worker_rows(p) if row["id"] in configured]
    return {
        "ok": True,
        "configured_workers": configured,
        "worker_count": 5,
        "workers": rows,
    }


def cmd_stop(p: Paths, args: argparse.Namespace) -> dict[str, Any]:
    state = load_state(p)
    targets = set(args.ids or state.get("workers", {}).keys())
    stopped: list[str] = []
    for worker_id, item in state.get("workers", {}).items():
        if worker_id not in targets or not alive(item.get("pid")):
            continue
        try:
            os.killpg(item["pid"], signal.SIGTERM)
            stopped.append(worker_id)
        except ProcessLookupError:
            pass
    return {"ok": True, "stopped": stopped}


def copy_overlay(source: Path, destination: Path) -> int:
    count = 0
    for path in sorted(source.rglob("*")):
        if path.is_symlink():
            raise LabError(f"promotion overlay may not contain symlinks: {path}")
        if path.is_dir():
            continue
        relative = path.relative_to(source)
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, target)
        count += 1
    if count == 0:
        raise LabError(f"promotion overlay is empty: {source}")
    return count


def verifier_runtime_path(path: str) -> bool:
    exact = {
        ".prolog/facts.kb",
        ".prolog/verify.pl",
        ".prolog/result.json",
        ".prolog/.facts.lock",
    }
    return path in exact or path.startswith(".prolog/sessions/") or path.startswith(".prolog/runs/")


def candidate_dirty_paths(worktree: Path) -> list[str]:
    tracked = git(worktree, "diff", "--name-only", "HEAD", "--").stdout.splitlines()
    untracked = git(worktree, "ls-files", "--others", "--exclude-standard").stdout.splitlines()
    return sorted({path for path in [*tracked, *untracked] if not verifier_runtime_path(path)})


def cmd_promote(p: Paths, args: argparse.Namespace) -> dict[str, Any]:
    require_executable("git")
    require_executable("python3")
    require_executable("prolog-verify")
    validate_repo(p.dotfiles, "Dotfiles")
    validate_repo(p.zara_plugins, "zara-plugins")
    specs = {item["id"]: item for item in load_specs(p.dotfiles)}
    if args.id not in specs:
        raise LabError(f"unknown feature id: {args.id}")
    spec = specs[args.id]
    state = load_state(p)
    worker = state.get("workers", {}).get(args.id)
    if not worker:
        raise LabError(f"feature has not been started: {args.id}")
    if alive(worker.get("pid")):
        raise LabError(f"worker is still running: {args.id}")

    worktree = expand(worker["worktree"])
    dirty = candidate_dirty_paths(worktree)
    if dirty:
        raise LabError(
            "worker candidate has uncommitted feature changes: " + ", ".join(dirty[:20])
        )
    run(["prolog-verify", "--work-dir", str(worktree), "check"])

    overlay = worktree / ".zara/labs/zara-feature-lab/features" / args.id / "overlay"
    stamp = time.strftime("%Y%m%d-%H%M%S")
    promotion_branch = f"promote/zara-lab-{args.id}-{stamp}"
    promotion_worktree = p.state / "promotions" / f"{args.id}-{stamp}"
    base = git(p.zara_plugins, "rev-parse", "main").stdout.strip()
    ensure_worktree(p.zara_plugins, promotion_worktree, promotion_branch, base)
    target = promotion_worktree / spec["promotion_path"]
    copied = copy_overlay(overlay, target)

    validate = promotion_worktree / "scripts/validate-registry.py"
    if validate.is_file():
        run(["python3", str(validate)], cwd=promotion_worktree)
    test_dir = promotion_worktree / spec["test_dir"]
    if test_dir.is_dir():
        relative = str(test_dir.relative_to(promotion_worktree))
        run(["python3", "-m", "unittest", "discover", "-s", relative, "-t", relative], cwd=promotion_worktree)

    return {
        "ok": True,
        "feature": args.id,
        "copied_files": copied,
        "promotion_branch": promotion_branch,
        "promotion_worktree": str(promotion_worktree),
        "target_plugin": spec["target_plugin"],
        "merged": False,
    }


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="zara-feature-lab")
    sub = p.add_subparsers(dest="command", required=True)
    sub.add_parser("start", help="admit and launch all five isolated workers")
    sub.add_parser("status", help="show configured and running workers")
    stop = sub.add_parser("stop", help="stop selected workers or all workers")
    stop.add_argument("ids", nargs="*")
    promote = sub.add_parser("promote", help="copy one verified overlay into an isolated zara-plugins worktree")
    promote.add_argument("id")
    return p


def main() -> int:
    args = parser().parse_args()
    p = paths()
    try:
        if args.command == "start":
            result = cmd_start(p, args)
        elif args.command == "status":
            result = cmd_status(p, args)
        elif args.command == "stop":
            result = cmd_stop(p, args)
        elif args.command == "promote":
            result = cmd_promote(p, args)
        else:
            raise LabError(f"unsupported command: {args.command}")
        print(json.dumps(result, sort_keys=True))
        return 0
    except (LabError, OSError, subprocess.SubprocessError) as exc:
        print(json.dumps({"ok": False, "error": str(exc)}, sort_keys=True))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
