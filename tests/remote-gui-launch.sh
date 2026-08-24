#!/usr/bin/env bash
set -euo pipefail

readonly repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
readonly launcher="$repo_root/scripts/remote-gui-launch"
readonly tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cat > "$tmp/ssh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "${SSH_ARGS:?}"
cat > "${SSH_STDIN:?}"
exit "${SSH_EXIT:-0}"
EOF
chmod 0755 "$tmp/ssh"

export PATH="$tmp:$PATH"
export SSH_ARGS="$tmp/ssh.args"
export SSH_STDIN="$tmp/ssh.stdin"

REMOTE_GUI_SSH_TARGET='flake-test' \
  REMOTE_GUI_DESKTOP_ENTRY='chatgpt.desktop' \
  REMOTE_GUI_TMUX_SESSION='chatgpt-test' \
  bash "$launcher"

grep -Fxq -- '-o' "$SSH_ARGS"
grep -Fxq 'ServerAliveInterval=5' "$SSH_ARGS"
grep -Fxq 'ServerAliveCountMax=6' "$SSH_ARGS"
grep -Fxq 'flake-test' "$SSH_ARGS"
grep -Fq 'chatgpt.desktop' "$SSH_STDIN"
grep -Fq 'chatgpt-test' "$SSH_STDIN"
grep -Fq 'systemctl --user show-environment' "$SSH_STDIN"
grep -Fq 'tmux new-session -d' "$SSH_STDIN"
grep -Fq 'DBUS_SESSION_BUS_ADDRESS' "$SSH_STDIN"
grep -Fq 'XAUTHORITY' "$SSH_STDIN"

printf 'remote-gui-launch tests passed\n'
