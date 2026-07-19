#!/usr/bin/env bash
set -Eeuo pipefail

usage() {
  cat <<'EOF'
Usage: deploy-xdg-links.sh [--dry-run]

Create real XDG directories and link tracked dotfile entries into them.
The repository is derived from this script unless DOTFILES_ROOT is set.
HOME and the XDG_*_HOME variables select the destination tree.
EOF
}

dry_run=false
while (($#)); do
  case "$1" in
    --dry-run|-n)
      dry_run=true
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown argument: %s\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="${DOTFILES_ROOT:-$(cd -- "$script_dir/.." && pwd)}"
config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
data_home="${XDG_DATA_HOME:-$HOME/.local/share}"
state_home="${XDG_STATE_HOME:-$HOME/.local/state}"
cache_home="${XDG_CACHE_HOME:-$HOME/.cache}"
bin_home="$HOME/.local/bin"

[[ -d "$repo_root/.git" ]] || {
  printf 'Not a Git worktree: %s\n' "$repo_root" >&2
  exit 1
}

run() {
  printf '+'
  printf ' %q' "$@"
  printf '\n'
  $dry_run || "$@"
}

ensure_real_directory() {
  local directory=$1

  if [[ -L "$directory" ]]; then
    printf 'Refusing XDG directory symlink: %s -> %s\n' \
      "$directory" "$(readlink -- "$directory")" >&2
    return 1
  fi
  if [[ -e "$directory" && ! -d "$directory" ]]; then
    printf 'Refusing non-directory XDG path: %s\n' "$directory" >&2
    return 1
  fi
  [[ -d "$directory" ]] || run mkdir -p -- "$directory"
}

link_managed_file() {
  local source=$1 destination=$2 parent existing

  parent="$(dirname -- "$destination")"
  ensure_real_directory "$parent"

  if [[ -L "$destination" ]]; then
    existing="$(realpath -m -- "$destination")"
    if [[ "$existing" == "$(realpath -m -- "$source")" ]]; then
      printf '= %s -> %s\n' "$destination" "$(readlink -- "$destination")"
      return
    fi
    printf 'Refusing unmanaged link: %s -> %s\n' \
      "$destination" "$(readlink -- "$destination")" >&2
    return 1
  fi
  if [[ -e "$destination" ]]; then
    printf 'Refusing existing path: %s\n' "$destination" >&2
    return 1
  fi

  run ln -s -- "$source" "$destination"
}

for root in "$config_home" "$HOME/.local" "$bin_home" "$data_home" \
  "$state_home" "$cache_home"; do
  ensure_real_directory "$root"
done

while IFS= read -r -d '' tracked; do
  case "$tracked" in
    .config/dunstrc|.config/starintel/*.ini)
      continue
      ;;
  esac
  link_managed_file \
    "$repo_root/$tracked" \
    "$config_home/${tracked#.config/}"
done < <(git -C "$repo_root" ls-files -z '.config/**')

while IFS= read -r -d '' tracked; do
  link_managed_file \
    "$repo_root/$tracked" \
    "$data_home/${tracked#.local/share/}"
done < <(git -C "$repo_root" ls-files -z '.local/share/**')
