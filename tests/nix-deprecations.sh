#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
nix_dir="$repo_root/nix"

fail() {
  printf 'nix-deprecations: %s\n' "$*" >&2
  exit 1
}

[[ -d "$nix_dir" ]] || fail "missing nix directory"

# Deprecated attribute accesses that must stay out of repository-owned Nix
# expressions. Comment lines are allowed to mention the old names.
check_pattern() {
  local label="$1"
  local pattern="$2"
  local match
  while IFS= read -r match; do
    [[ -n "$match" ]] || continue
    local line="${match#*:}"
    local trimmed="${line#"${line%%[![:space:]]*}"}"
    [[ "$trimmed" == \#* ]] || fail "$label uses deprecated reference: $match"
  done < <(grep -rEn "$pattern" "$nix_dir" --include='*.nix' || true)
}

check_pattern "pkgs.system" '\$\{?pkgs\.system\}?'
check_pattern "stdenv.isLinux" 'stdenv\.isLinux'
check_pattern "xorg package set" 'xorg\.[a-zA-Z0-9_-]+'
check_pattern "input defaultPackage" 'inputs\.[a-zA-Z0-9_-]+\.defaultPackage'

printf 'nix-deprecations: ok\n'
