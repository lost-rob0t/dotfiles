#!/usr/bin/env bash
set -Eeuo pipefail

# Structural gate for the personal Brave Goggles profiles shipped by
# nix/home-manager/mods/brave-mcp.nix.  Deterministic and offline: it never
# queries the Brave API, it validates the .goggle sources and their wiring.

ROOT=${DOTFILES_ROOT:-$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)}
GOGGLES_DIR="$ROOT/nix/home-manager/files/goggles"
MODULE="$ROOT/nix/home-manager/mods/brave-mcp.nix"

# Brave requires this metadata header in every Goggle file.
check_metadata() {
  local goggle=$1
  grep -q '^! name: ' "$goggle"
  grep -q '^! description: ' "$goggle"
  grep -q '^! public: ' "$goggle"
  grep -q '^! author: ' "$goggle"
}

# Instruction grammar: action-first with optional site option, or a path
# pattern trigger carrying an action.  Strengths stay in the 1-10 range.
check_instruction() {
  local line=$1 strength=
  (( ${#line} <= 500 )) || {
    echo "instruction longer than 500 chars: $line" >&2
    return 1
  }
  if [[ "$line" =~ ^/ ]]; then
    [[ "$line" =~ ^/[^$]*\$(boost|downrank|discard)(=([0-9]+))?$ ]]
    strength=${BASH_REMATCH[3]:-}
  else
    [[ "$line" =~ ^\$(boost|downrank|discard)(=([0-9]+))?(,site=[a-zA-Z0-9.-]+)?$ ]]
    strength=${BASH_REMATCH[3]:-}
  fi
  if [[ -n "$strength" ]]; then
    (( 10#${strength} >= 1 && 10#${strength} <= 10 )) || {
      echo "strength out of range 1-10: $line" >&2
      return 1
    }
  fi
}

shopt -u nullglob
goggles=("$GOGGLES_DIR"/*.goggle)
shopt -u nullglob
(( ${#goggles[@]} > 0 )) || {
  echo "no .goggle files under $GOGGLES_DIR" >&2
  exit 1
}

for goggle in "${goggles[@]}"; do
  name=$(basename "$goggle")
  check_metadata "$goggle"
  while IFS= read -r line; do
    check_instruction "$line"
  done < <(grep -v '^!' "$goggle" | grep -v '^[[:space:]]*$')
done

# Every shipped profile must be wired into the HM module, and every wired
# name must exist on disk.
for goggle in "${goggles[@]}"; do
  grep -qF "brave/goggles/$(basename "$goggle")" "$MODULE"
done
while IFS= read -r wired; do
  [[ -f "$GOGGLES_DIR/$wired" ]] || {
    echo "$MODULE deploys a missing goggle: $wired" >&2
    exit 1
  }
done < <(grep -oP 'brave/goggles/\K[a-z0-9.-]+\.goggle' "$MODULE" | sort -u)

echo "brave-goggles: ${#goggles[@]} profile(s) valid and wired"
