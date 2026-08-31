#!/usr/bin/env bash
# Sync a flake input (default: skills) in the dotfiles checkout, publish the
# checked lockfile, and activate the local Home Manager configuration.
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: skill-sync [OPTIONS]

Update the skills flake input, validate the flake, publish the lockfile, and
activate the local Home Manager configuration.

Environment:
  SKILL_SYNC_REPO    Dotfiles checkout (default: ~/.dotfiles)

Options:
  --repo DIR            Dotfiles git checkout
  --input NAME          Flake input to update (default: skills)
  --branch NAME         Default branch to sync and push (default: origin/HEAD, else master)
  --configuration NAME  Home Manager target (default: auto-discovered USER@HOST,
                        otherwise the sole exported configuration)
  --dry-run             Print the plan without changing anything
  --no-push             Commit the lockfile locally but skip pushing
  --no-activate         Skip the Home Manager build and switch
  -h, --help            Show this help

The lockfile is only published after `nix flake check -L` succeeds, mirroring
the repository's daily flake-update workflow.
EOF
}

log() {
  printf '[skill-sync] %s\n' "$*"
}

die() {
  printf '[skill-sync] ERROR: %s\n' "$*" >&2
  exit 1
}

repo="${SKILL_SYNC_REPO:-$HOME/.dotfiles}"
input="skills"
branch=""
configuration=""
dry_run=0
no_push=0
no_activate=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo) repo="${2:?--repo needs a value}"; shift 2 ;;
    --input) input="${2:?--input needs a value}"; shift 2 ;;
    --branch) branch="${2:?--branch needs a value}"; shift 2 ;;
    --configuration) configuration="${2:?--configuration needs a value}"; shift 2 ;;
    --dry-run) dry_run=1; shift ;;
    --no-push) no_push=1; shift ;;
    --no-activate) no_activate=1; shift ;;
    -h | --help) usage; exit 0 ;;
    *) printf '[skill-sync] unknown option: %s\n\n' "$1" >&2; usage >&2; exit 2 ;;
  esac
done

[[ -d "$repo" ]] || die "repository not found: $repo"
repo="$(cd "$repo" && pwd)"
[[ -f "$repo/flake.nix" ]] || die "no flake.nix in $repo"
[[ -f "$repo/flake.lock" ]] || die "no flake.lock in $repo"
for tool in git jq nix; do
  command -v "$tool" >/dev/null 2>&1 || die "required tool not found: $tool"
done
if (( ! no_activate )); then
  command -v home-manager >/dev/null 2>&1 || die "home-manager not found; pass --no-activate to skip activation"
fi

git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1 || die "not a git checkout: $repo"

locked_rev() {
  jq -r --arg input "$input" '.nodes[$input].locked.rev // empty' "$repo/flake.lock"
}

git -C "$repo" fetch --prune origin
if [[ -z "$branch" ]]; then
  branch="$(git -C "$repo" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's|^origin/||' || true)"
  branch="${branch:-master}"
fi
current_branch="$(git -C "$repo" branch --show-current)"
if [[ "$current_branch" != "$branch" ]]; then
  die "current branch is '$current_branch', expected '$branch'; refusing to sync"
fi
if [[ -n "$(git -C "$repo" status --porcelain -- flake.lock)" ]]; then
  die "flake.lock has uncommitted changes; refusing to sync"
fi
jq -e --arg input "$input" '.nodes | has($input)' "$repo/flake.lock" >/dev/null \
  || die "flake input '$input' is not locked in flake.lock"
old_rev="$(locked_rev)"
[[ -n "$old_rev" ]] || die "flake input '$input' has no locked revision"

if (( dry_run )); then
  log "dry-run: would fast-forward $branch to origin/$branch"
  log "dry-run: would update input '$input' (locked at ${old_rev:0:12})"
  log "dry-run: would gate on: nix flake check -L --show-trace"
  if (( no_push )); then
    log "dry-run: would commit flake.lock locally (push disabled)"
  else
    log "dry-run: would commit flake.lock and push to origin/$branch"
  fi
  if (( no_activate )); then
    log "dry-run: activation skipped (--no-activate)"
  else
    log "dry-run: would build and switch the discovered Home Manager configuration"
  fi
  exit 0
fi

git -C "$repo" merge --ff-only "origin/$branch"
(cd "$repo" && nix flake update "$input")
new_rev="$(locked_rev)"
[[ -n "$new_rev" ]] || die "flake input '$input' has no locked revision after update"

if [[ "$new_rev" == "$old_rev" ]]; then
  log "Input '$input' is already current at ${old_rev:0:12}; nothing to do"
  exit 0
fi
log "Input '$input': ${old_rev:0:12} -> ${new_rev:0:12}"

log "Validating flake: nix flake check -L"
(cd "$repo" && nix flake check -L --show-trace)

git -C "$repo" add -- flake.lock
git -C "$repo" commit -m "chore(nix): sync $input input to ${new_rev:0:12}"
if (( no_push )); then
  log "Committed flake.lock locally; push skipped (--no-push)"
else
  git -C "$repo" push origin "HEAD:$branch"
  log "Pushed flake.lock to origin/$branch"
fi

if (( no_activate )); then
  log "Activation skipped (--no-activate)"
  exit 0
fi

if [[ -z "$configuration" ]]; then
  configs_json="$(nix eval --json "$repo#homeConfigurations" --apply 'builtins.attrNames')"
  mapfile -t configs < <(jq -r '.[]' <<<"$configs_json")
  want="$(id -un)@$(hostname -s)"
  for candidate in "${configs[@]}"; do
    if [[ "$candidate" == "$want" ]]; then
      configuration="$candidate"
      break
    fi
  done
  if [[ -z "$configuration" ]]; then
    if (( ${#configs[@]} == 1 )); then
      configuration="${configs[0]}"
    else
      die "could not discover a Home Manager configuration for '$want'; exported: ${configs[*]}. Pass --configuration."
    fi
  fi
fi

log "Building activation package for $configuration"
nix build --no-link "$repo#homeConfigurations.\"$configuration\".activationPackage"
log "Activating $configuration"
home-manager switch --flake "$repo#$configuration" --show-trace
log "Done: $input ${old_rev:0:12} -> ${new_rev:0:12}; configuration $configuration activated"
