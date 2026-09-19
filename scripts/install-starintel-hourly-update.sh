#!/usr/bin/env bash
set -euo pipefail

log() {
  printf '[install-starintel-hourly-update] %s\n' "$*"
}

die() {
  printf '[install-starintel-hourly-update] ERROR: %s\n' "$*" >&2
  exit 1
}

for tool in crontab nix-collect-garbage; do
  command -v "$tool" >/dev/null 2>&1 || die "required tool not found: $tool"
done

agent_zero_root="${AGENT_ZERO_ROOT:-}"
skills_repo="${STARINTEL_SKILLS_REPO:-$HOME/skills}"
dotfiles_repo="${STARINTEL_DOTFILES_REPO:-$HOME/.dotfiles}"
hm_configuration="${STARINTEL_HM_CONFIGURATION:-$(id -un)@$(hostname -s)}"
hourly_command="${STARINTEL_HOURLY_COMMAND:-$HOME/.nix-profile/bin/starintel-hourly-update}"
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/starintel"
state_dir="${XDG_STATE_HOME:-$HOME/.local/state}/starintel"
wrapper="$config_dir/starintel-hourly-update-cron"
log_file="$state_dir/hourly-update.log"
marker="# starintel-hourly-update"

[[ -n "$agent_zero_root" ]] || die "AGENT_ZERO_ROOT is required"
if [[ ! -x "$hourly_command" ]]; then
  hourly_command="$(command -v starintel-hourly-update || true)"
fi
[[ -n "$hourly_command" && -x "$hourly_command" ]] || die "starintel-hourly-update is not installed"

mkdir -p "$config_dir" "$state_dir"

log "running one Nix garbage-collection pass"
nix-collect-garbage

log "running initial skills install and Home Manager update"
AGENT_ZERO_ROOT="$agent_zero_root" \
STARINTEL_SKILLS_REPO="$skills_repo" \
STARINTEL_DOTFILES_REPO="$dotfiles_repo" \
STARINTEL_HM_CONFIGURATION="$hm_configuration" \
  "$hourly_command"

printf '#!/usr/bin/env bash\nset -euo pipefail\nexport AGENT_ZERO_ROOT=%q\nexport STARINTEL_SKILLS_REPO=%q\nexport STARINTEL_DOTFILES_REPO=%q\nexport STARINTEL_HM_CONFIGURATION=%q\nexec %q\n' \
  "$agent_zero_root" "$skills_repo" "$dotfiles_repo" "$hm_configuration" "$hourly_command" >"$wrapper"
chmod 700 "$wrapper"

existing="$(crontab -l 2>/dev/null || true)"
filtered="$(printf '%s\n' "$existing" | grep -Fv "$marker" || true)"
{
  [[ -z "$filtered" ]] || printf '%s\n' "$filtered"
  printf '0 * * * * %q >> %q 2>&1 %s\n' "$wrapper" "$log_file" "$marker"
} | crontab -

log "installed hourly cron entry"
log "cron wrapper: $wrapper"
log "log file: $log_file"
