#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/nix/home-manager/files/zarathushtra/bin/zara-dev"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mockbin="$tmp/bin"
mkdir -p "$mockbin"

cat >"$mockbin/systemctl" <<'EOF'
#!/usr/bin/env bash
set -u
printf '%s\n' "$*" >>"${MOCK_SYSTEMCTL_LOG:?}"
if [[ "$1 $2" == "--user show-environment" ]]; then
  exit 0
fi
exit 0
EOF

chmod +x "$mockbin"/*
export PATH="$mockbin:$PATH"

fail_test() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

assert_contains() {
  local file="$1"
  local expected="$2"
  grep -Fq -- "$expected" "$file" || fail_test "$file does not contain: $expected"
}

assert_absent() {
  local file="$1"
  local unexpected="$2"
  ! grep -Fq -- "$unexpected" "$file" || fail_test "$file unexpectedly contains: $unexpected"
}

reset_case() {
  export MOCK_SYSTEMCTL_LOG="$tmp/$1.log"
  : >"$MOCK_SYSTEMCTL_LOG"
}

zara_units="zara-server.service zara-pair.service zara-desktop.service zara-wake.service"

reset_case default-off
bash "$script"
for unit in $zara_units; do
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user stop $unit"
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user disable $unit"
done
assert_absent "$MOCK_SYSTEMCTL_LOG" "qwen3-tts.service"

reset_case off-with-tts
bash "$script" off --with-tts
for unit in $zara_units qwen3-tts.service; do
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user stop $unit"
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user disable $unit"
done

reset_case on-with-tts
bash "$script" on --with-tts
for unit in $zara_units qwen3-tts.service; do
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user enable --now $unit"
done
assert_absent "$MOCK_SYSTEMCTL_LOG" "--user stop"

reset_case status
bash "$script" status
for unit in $zara_units; do
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user is-enabled $unit"
  assert_contains "$MOCK_SYSTEMCTL_LOG" "--user is-active $unit"
done
assert_absent "$MOCK_SYSTEMCTL_LOG" "--user stop"
assert_absent "$MOCK_SYSTEMCTL_LOG" "--user disable"
assert_absent "$MOCK_SYSTEMCTL_LOG" "--user enable"

reset_case bad-arg
if bash "$script" bogus >/dev/null 2>&1; then
  fail_test "zara-dev accepted an unknown argument"
fi
[[ ! -s "$MOCK_SYSTEMCTL_LOG" ]] || fail_test "zara-dev touched units before validating arguments"

printf 'zara-dev tests: PASS\n'
