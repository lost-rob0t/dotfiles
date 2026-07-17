#!/usr/bin/env bash
set -Eeuo pipefail

usage() {
  echo "usage: sudo $0 /dev/DEVICE" >&2
  exit 2
}

[[ $# -eq 1 ]] || usage
[[ $EUID -eq 0 ]] || { echo "run as root" >&2; exit 1; }

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
TARGET_ROOT="${TARGET_ROOT:-/mnt}"
TARGET_CONFIG="$TARGET_ROOT/etc/nixos"

"$SCRIPT_DIR/provision-logos-disk.sh" "$1"

rm -rf "$TARGET_CONFIG"
mkdir -p "$TARGET_CONFIG"
cp -a "$REPO_ROOT"/. "$TARGET_CONFIG"/

nixos-generate-config \
  --root "$TARGET_ROOT" \
  --show-hardware-config \
  > "$TARGET_CONFIG/nix/nixos/hosts/logos/hardware-configuration.nix"

nixos-install \
  --root "$TARGET_ROOT" \
  --flake "$TARGET_CONFIG#logos"

cat <<EOF

Logos is installed.
Set the user password before rebooting:
  nixos-enter --root $TARGET_ROOT -c 'passwd unseen'

The installed flake is at:
  $TARGET_CONFIG
EOF
