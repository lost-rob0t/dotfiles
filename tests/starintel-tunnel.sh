#!/usr/bin/env bash
set -euo pipefail

script=${1:-.local/bin/starintel-tunnel}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin" "$tmp/home/.config/starintel"

org="${script}.org"
if [[ -r $org ]]; then
  awk '/^#\+begin_src bash :tangle starintel-tunnel$/ { capture=1; next } /^#\+end_src$/ && capture { exit } capture' "$org" >"$tmp/tangled"
  cmp -s "$tmp/tangled" "$script"
fi

cat >"$tmp/bin/ssh" <<'SH'
#!/usr/bin/env bash
exit 0
SH
chmod +x "$tmp/bin/ssh"

cat >"$tmp/bin/tmux" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
state=${TMUX_TEST_STATE:?}
log=${TMUX_TEST_LOG:?}
printf '%q ' "$@" >>"$log"
printf '\n' >>"$log"
case ${1:-} in
  has-session)
    session=${3:?}
    grep -Fxq "$session" "$state" 2>/dev/null
    ;;
  new-session)
    session=${4:?}
    printf '%s\n' "$session" >>"$state"
    ;;
  kill-session)
    session=${3:?}
    grep -Fxv "$session" "$state" >"$state.next" || true
    mv "$state.next" "$state"
    ;;
  attach-session)
    exit 0
    ;;
esac
SH
chmod +x "$tmp/bin/tmux"

cat >"$tmp/home/.config/starintel/tunnels.conf" <<'CONF'
# override one built-in and add one custom service
couchdb 15984 127.0.0.1 5984 bastion
api 18080 127.0.0.1 8080 starintel
CONF

export PATH="$tmp/bin:$PATH"
export HOME="$tmp/home"
export XDG_CONFIG_HOME="$tmp/home/.config"
export TMUX_TEST_STATE="$tmp/state"
export TMUX_TEST_LOG="$tmp/log"
: >"$TMUX_TEST_STATE"
: >"$TMUX_TEST_LOG"

output=$($script list)
grep -Fq 'couchdb          localhost:15984 -> 127.0.0.1:5984 via bastion' <<<"$output"
grep -Fq 'api              localhost:18080 -> 127.0.0.1:8080 via starintel' <<<"$output"

$script start all >/dev/null
for session in \
  starintel-tunnel-couchdb \
  starintel-tunnel-rabbitmq-ui \
  starintel-tunnel-rabbitmq-amqp \
  starintel-tunnel-valkey \
  starintel-tunnel-api; do
  grep -Fxq "$session" "$TMUX_TEST_STATE"
done

grep -Fq 'ExitOnForwardFailure=yes' "$TMUX_TEST_LOG"
grep -Fq '15984:127.0.0.1:5984' "$TMUX_TEST_LOG"

after_first=$(wc -l <"$TMUX_TEST_STATE")
$script start all >/dev/null
after_second=$(wc -l <"$TMUX_TEST_STATE")
[[ $after_first -eq $after_second ]]

$script stop couchdb >/dev/null
! grep -Fxq 'starintel-tunnel-couchdb' "$TMUX_TEST_STATE"

printf 'starintel-tunnel tests: ok\n'
