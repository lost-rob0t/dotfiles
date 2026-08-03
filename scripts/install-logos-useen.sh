#!/usr/bin/env bash
set -Eeuo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
temporary_script="$(mktemp)"
trap 'rm -f -- "$temporary_script"' EXIT

sed 's/\bunseen\b/useen/g' "$script_dir/install-logos.sh" > "$temporary_script"
bash "$temporary_script" "$@"
