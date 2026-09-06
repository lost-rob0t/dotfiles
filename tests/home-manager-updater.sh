#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
updater="$repo_root/scripts/home-manager-updater.sh"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mockbin="$tmp/bin"
mkdir -p "$mockbin"

cat >"$mockbin/git" <<'EOF'
#!/usr/bin/env bash
set -u
if [[ "$*" == *"rev-parse HEAD"* ]]; then
  printf 'deadbeef\n'
fi
exit 0
EOF

cat >"$mockbin/nix" <<'EOF'
#!/usr/bin/env bash
exit "${MOCK_NIX_RC:-0}"
EOF

cat >"$mockbin/home-manager" <<'EOF'
#!/usr/bin/env bash
set -u
state="${MOCK_GENERATION_STATE:?}"
counts="${MOCK_COUNTS:?}"
old="${MOCK_OLD_GENERATION:?}"
new="${MOCK_NEW_GENERATION:?}"

if [[ "${1:-}" == "generations" ]]; then
  if [[ "${MOCK_GENERATIONS_RC:-0}" -ne 0 ]]; then
    exit "$MOCK_GENERATIONS_RC"
  fi
  printf '2026-08-21 00:00 : id 1 -> %s\n' "$(cat "$state")"
  exit 0
fi

if [[ "${1:-}" == "switch" && "${2:-}" == "--rollback" ]]; then
  printf 'rollback\n' >>"$counts"
  if [[ "${MOCK_ROLLBACK_RC:-0}" -eq 0 ]]; then
    printf '%s\n' "$old" >"$state"
  fi
  exit "${MOCK_ROLLBACK_RC:-0}"
fi

if [[ "${1:-}" == "switch" ]]; then
  printf 'switch\n' >>"$counts"
  if [[ "${MOCK_ADVANCE_ON_SWITCH:-0}" -eq 1 ]]; then
    printf '%s\n' "$new" >"$state"
  fi
  exit "${MOCK_SWITCH_RC:-0}"
fi

exit 2
EOF

cat >"$mockbin/tea" <<'EOF'
#!/usr/bin/env bash
# Tests deliberately model an unauthenticated Forgejo client so issue reporting
# stays best-effort and cannot interfere with the recovery assertions.
exit 1
EOF

cat >"$mockbin/logrotate" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF

cat >"$mockbin/notify-send" <<'EOF'
#!/usr/bin/env bash
if [[ "$*" == *"--print-id"* ]]; then
  printf '42\n'
fi
exit 0
EOF

chmod +x "$mockbin"/*

export PATH="$mockbin:$PATH"
export HM_UPDATER_REPOSITORY="lost-rob0t/dotfiles"
export HM_UPDATER_REMOTE_URL="git@git.starintel.actor:lost-rob0t/dotfiles.git"
export HM_UPDATER_BRANCH="master"
export HM_UPDATER_CONFIGURATION="unseen@flake"
export HM_UPDATER_LOGROTATE_CONFIG="$tmp/logrotate.conf"
export MOCK_OLD_GENERATION="/nix/store/old-home-manager-generation"
export MOCK_NEW_GENERATION="/nix/store/new-home-manager-generation"

printf 'unused\n' >"$HM_UPDATER_LOGROTATE_CONFIG"

fail_test() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

assert_contains() {
  local file="$1"
  local expected="$2"
  grep -Fq -- "$expected" "$file" || fail_test "$file does not contain: $expected"
}

assert_count() {
  local needle="$1"
  local expected="$2"
  local actual
  actual="$(grep -c -x "$needle" "$MOCK_COUNTS" 2>/dev/null || true)"
  [[ "$actual" == "$expected" ]] || fail_test "expected $expected '$needle' calls, got $actual"
}

reset_case() {
  local name="$1"
  export HM_UPDATER_DATA_DIR="$tmp/$name/data"
  export MOCK_GENERATION_STATE="$tmp/$name/generation"
  export MOCK_COUNTS="$tmp/$name/counts"
  mkdir -p "$HM_UPDATER_DATA_DIR/repo/.git"
  printf '%s\n' "$MOCK_OLD_GENERATION" >"$MOCK_GENERATION_STATE"
  : >"$MOCK_COUNTS"
  export MOCK_NIX_RC=0
  export MOCK_GENERATIONS_RC=0
  export MOCK_SWITCH_RC=0
  export MOCK_ROLLBACK_RC=0
  export MOCK_ADVANCE_ON_SWITCH=0
}

run_expect_failure() {
  if bash "$updater"; then
    fail_test "updater unexpectedly succeeded"
  fi
}

reset_case build-failure
export MOCK_NIX_RC=1
run_expect_failure
assert_count switch 0
assert_count rollback 0
[[ "$(cat "$MOCK_GENERATION_STATE")" == "$MOCK_OLD_GENERATION" ]] || fail_test "build failure changed generation"
assert_contains "$HM_UPDATER_DATA_DIR/failure.txt" "Stage: build"

reset_case missing-rollback-baseline
export MOCK_GENERATIONS_RC=1
run_expect_failure
assert_count switch 0
assert_count rollback 0
assert_contains "$HM_UPDATER_DATA_DIR/failure.txt" "Stage: preflight"
assert_contains "$HM_UPDATER_DATA_DIR/failure.txt" "no trustworthy rollback baseline"

reset_case activation-before-generation
export MOCK_SWITCH_RC=1
run_expect_failure
assert_count switch 1
assert_count rollback 0
[[ "$(cat "$MOCK_GENERATION_STATE")" == "$MOCK_OLD_GENERATION" ]] || fail_test "unchanged activation failure changed generation"
assert_contains "$HM_UPDATER_DATA_DIR/failure.txt" "no rollback was necessary"

reset_case activation-after-generation
export MOCK_SWITCH_RC=1
export MOCK_ADVANCE_ON_SWITCH=1
run_expect_failure
assert_count switch 1
assert_count rollback 1
[[ "$(cat "$MOCK_GENERATION_STATE")" == "$MOCK_OLD_GENERATION" ]] || fail_test "rollback did not restore previous generation"
assert_contains "$HM_UPDATER_DATA_DIR/failure.txt" "previous Home Manager generation was restored successfully"

reset_case success
export MOCK_ADVANCE_ON_SWITCH=1
printf 'stale failure\n' >"$HM_UPDATER_DATA_DIR/failure.txt"
printf '42\n' >"$HM_UPDATER_DATA_DIR/notification-id"
bash "$updater"
assert_count switch 1
assert_count rollback 0
[[ ! -e "$HM_UPDATER_DATA_DIR/failure.txt" ]] || fail_test "success did not clear failure marker"
[[ ! -e "$HM_UPDATER_DATA_DIR/notification-id" ]] || fail_test "success did not clear notification id"
[[ "$(cat "$MOCK_GENERATION_STATE")" == "$MOCK_NEW_GENERATION" ]] || fail_test "success did not advance generation"

printf 'home-manager-updater tests: PASS\n'
