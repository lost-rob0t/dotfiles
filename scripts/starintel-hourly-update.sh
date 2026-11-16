#!/usr/bin/env bash
set -euo pipefail

log() {
  printf '[starintel-hourly-update] %s\n' "$*"
}

die() {
  printf '[starintel-hourly-update] ERROR: %s\n' "$*" >&2
  exit 1
}

skills_repo="${STARINTEL_SKILLS_REPO:-$HOME/skills}"
dotfiles_repo="${STARINTEL_DOTFILES_REPO:-$HOME/.dotfiles}"
agent_zero_root="${AGENT_ZERO_ROOT:-}"
hm_configuration="${STARINTEL_HM_CONFIGURATION:-$(id -un)@$(hostname -s)}"
lock_file="${XDG_RUNTIME_DIR:-/tmp}/starintel-hourly-update.lock"

for tool in git home-manager flock; do
  command -v "$tool" >/dev/null 2>&1 || die "required tool not found: $tool"
done
[[ -n "$agent_zero_root" ]] || die "AGENT_ZERO_ROOT is required"
[[ -d "$skills_repo/.git" ]] || die "skills checkout not found: $skills_repo"
[[ -d "$dotfiles_repo/.git" ]] || die "dotfiles checkout not found: $dotfiles_repo"

exec 9>"$lock_file"
if ! flock -n 9; then
  log "another update is running; exiting"
  exit 0
fi

ff_update() {
  local repo="$1"
  local label="$2"
  local branch

  if [[ -n "$(git -C "$repo" status --porcelain)" ]]; then
    die "$label checkout is dirty; refusing to overwrite local work: $repo"
  fi

  branch="$(git -C "$repo" branch --show-current)"
  [[ -n "$branch" ]] || die "$label checkout is detached: $repo"

  log "updating $label ($branch)"
  git -C "$repo" fetch --prune origin
  git -C "$repo" merge --ff-only "origin/$branch"
}

install_agent_zero_skills() {
  local source_root="$skills_repo/skills"
  local target_root="$agent_zero_root/usr/skills"
  local source name target existing

  [[ -d "$source_root" ]] || die "canonical skills directory missing: $source_root"
  mkdir -p "$target_root"

  for source in "$source_root"/*; do
    [[ -d "$source" && -f "$source/SKILL.md" ]] || continue
    name="$(basename "$source")"
    target="$target_root/$name"

    if [[ -L "$target" ]]; then
      existing="$(readlink -f "$target" || true)"
      if [[ "$existing" == "$(readlink -f "$source")" ]]; then
        continue
      fi
      rm -- "$target"
    elif [[ -e "$target" ]]; then
      die "refusing to replace non-symlink Agent Zero skill: $target"
    fi

    ln -s "$source" "$target"
    log "installed Agent Zero skill: $name"
  done
}

ff_update "$skills_repo" "skills"
install_agent_zero_skills
ff_update "$dotfiles_repo" "dotfiles"

log "activating Home Manager configuration $hm_configuration"
home-manager switch --flake "$dotfiles_repo#$hm_configuration" --show-trace
log "complete"
