#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

config="$tmpdir/tunnels.conf"
log="$tmpdir/ssh.log"
mkdir -p "$tmpdir/bin"

cat >"$config" <<'EOF'
# NAME LOCAL_PORT REMOTE_HOST REMOTE_PORT SSH_TARGET
couchdb 5984 127.0.0.1 5984 starintel
rabbitmq-ui 15672 127.0.0.1 15672 starintel
EOF

cat >"$tmpdir/bin/ssh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >>"$STARINTEL_TUNNEL_TEST_LOG"
EOF
chmod +x "$tmpdir/bin/ssh"

list_output="$({
  PATH="$tmpdir/bin:$PATH" \
    STARINTEL_TUNNELS_CONFIG="$config" \
    "$repo_root/scripts/starintel-tunnel" list
})"

grep -q '^couchdb' <<<"$list_output"
grep -q '^rabbitmq-ui' <<<"$list_output"

: >"$log"
PATH="$tmpdir/bin:$PATH" \
  STARINTEL_TUNNELS_CONFIG="$config" \
  STARINTEL_TUNNEL_TEST_LOG="$log" \
  "$repo_root/scripts/starintel-tunnel" couchdb

grep -Fq -- '-o ExitOnForwardFailure=yes -N -L 5984:127.0.0.1:5984 starintel' "$log"

cat >"$config" <<'EOF'
couchdb 5984 127.0.0.1 5984 starintel
couchdb 15984 127.0.0.1 5984 starintel
EOF

if PATH="$tmpdir/bin:$PATH" \
  STARINTEL_TUNNELS_CONFIG="$config" \
  "$repo_root/scripts/starintel-tunnel" list >"$tmpdir/duplicate.out" 2>"$tmpdir/duplicate.err"; then
  printf 'expected duplicate tunnel config to fail\n' >&2
  exit 1
fi

grep -Fq "duplicate tunnel name 'couchdb'" "$tmpdir/duplicate.err"

duplicates="$(
  awk 'NF && $1 !~ /^#/ { print $1 }' "$repo_root/.config/starintel/tunnels.conf" \
    | sort \
    | uniq -d
)"

if [[ -n "$duplicates" ]]; then
  printf 'duplicate tunnel names in repo config:\n%s\n' "$duplicates" >&2
  exit 1
fi

printf 'starintel-tunnel tests passed\n'
