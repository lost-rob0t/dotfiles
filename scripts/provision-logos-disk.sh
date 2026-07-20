#!/usr/bin/env bash
set -Eeuo pipefail

usage() {
  cat <<'USAGE'
provision-logos has been retired.

Disk formatting and installation are now one flake-native Disko operation.

Use:
  install-logos --help
  sudo install-logos --disk /dev/DEVICE
  sudo nix run .#install-logos -- --disk /dev/DEVICE
USAGE
}

usage >&2

case "${1-}" in
  -h | --help)
    exit 0
    ;;
  *)
    exit 2
    ;;
esac
