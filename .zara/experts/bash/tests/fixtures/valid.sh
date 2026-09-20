#!/usr/bin/env bash
set -euo pipefail

name="${1:-world}"
items=("a b" "c")

printf 'hello %s\n' "$name"
for item in "${items[@]}"; do
  printf '%s\n' "$item"
done

cat <<'EOF'
literal $HOME
EOF
