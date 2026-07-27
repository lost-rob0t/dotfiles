#!/usr/bin/env bash
set -Eeuo pipefail

readonly ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
readonly ACTION=${1:-apply}

case "$ACTION" in
  apply)
    bash "$ROOT/apply-outrun.sh" apply
    bash "$ROOT/fix-transparent-sidebar.sh"
    ;;
  restore)
    shift || true
    bash "$ROOT/apply-outrun.sh" restore "$@"
    ;;
  *)
    printf 'usage: bash ubuntu/apply.sh [apply|restore [backup-directory]]\n' >&2
    exit 1
    ;;
esac
