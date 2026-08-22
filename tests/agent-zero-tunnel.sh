#!/usr/bin/env bash
set -euo pipefail

readonly repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
readonly tunnel="$repo_root/scripts/agent-zero-tunnel"
readonly tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cat > "$tmp/ssh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "${SSH_CAPTURE:?}"
EOF
chmod 0755 "$tmp/ssh"

export PATH="$tmp:$PATH"
export SSH_CAPTURE="$tmp/ssh.args"

output="$(
  A0_SSH_TARGET='unseen@op.example' \
  A0_REMOTE_PORT=50080 \
  A0_LOCAL_PORT=55080 \
  bash "$tunnel"
)"

grep -Fxq 'Agent Zero: http://127.0.0.1:55080' <<< "$output"
grep -Fxq -- '-NT' "$SSH_CAPTURE"
grep -Fxq -- '-L' "$SSH_CAPTURE"
grep -Fxq '127.0.0.1:55080:127.0.0.1:50080' "$SSH_CAPTURE"
grep -Fxq 'ExitOnForwardFailure=yes' "$SSH_CAPTURE"
grep -Fxq 'ServerAliveInterval=30' "$SSH_CAPTURE"
grep -Fxq 'ServerAliveCountMax=3' "$SSH_CAPTURE"
grep -Fxq 'unseen@op.example' "$SSH_CAPTURE"

if A0_REMOTE_PORT=70000 bash "$tunnel" op >/dev/null 2>&1; then
  printf 'expected invalid port to fail\n' >&2
  exit 1
fi

printf 'agent-zero-tunnel tests passed\n'
