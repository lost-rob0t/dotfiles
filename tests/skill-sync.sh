#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/scripts/skill-sync.sh"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

old_rev="0000000000000000000000000000000000000000"
new_rev="1111111111111111111111111111111111111111"
mockbin="$tmp/bin"
mkdir -p "$mockbin"

cat >"$mockbin/nix" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  flake)
    case "${2:-}" in
      update)
        jq --arg new "${MOCK_NEW_REV:?}" '.nodes.skills.locked.rev = $new' flake.lock >flake.lock.tmp
        mv flake.lock.tmp flake.lock
        ;;
      check) exit "${MOCK_CHECK_RC:-0}" ;;
      *) exit 0 ;;
    esac
    ;;
  eval) printf '["unseen@flake"]\n' ;;
  build) exit "${MOCK_BUILD_RC:-0}" ;;
  *) exit 2 ;;
esac
EOF

cat >"$mockbin/home-manager" <<'EOF'
#!/usr/bin/env bash
if [[ "${1:-}" == "switch" ]]; then
  printf '%s\n' "$*" >>"${MOCK_HM_LOG:?}"
  exit "${MOCK_SWITCH_RC:-0}"
fi
exit 2
EOF

chmod +x "$mockbin/nix" "$mockbin/home-manager"

hm_log="$tmp/hm.log"
: >"$hm_log"

assert_eq() {
  local got="$1" want="$2" msg="$3"
  if [[ "$got" != "$want" ]]; then
    printf 'FAIL: %s\n  got:  %s\n  want: %s\n' "$msg" "$got" "$want" >&2
    exit 1
  fi
  printf 'ok: %s\n' "$msg"
}

make_fixture() {
  local name="$1"
  local fixture="$tmp/$name"
  : >"$hm_log"
  git init -q -b master "$fixture"
  printf '{"description":"fixture","inputs":{"skills":{"url":"github:example/skills"}},"outputs":{}}\n' >"$fixture/flake.nix"
  printf '{"nodes":{"skills":{"locked":{"rev":"%s"}}},"root":"","version":7}\n' "$old_rev" >"$fixture/flake.lock"
  git -C "$fixture" add -A
  git -C "$fixture" -c user.name=test -c user.email=test@example.test commit -qm init
  git init -q --bare "$tmp/$name-origin.git"
  git -C "$fixture" remote add origin "$tmp/$name-origin.git"
  git -C "$fixture" push -q origin master
  git -C "$fixture" config user.name test
  git -C "$fixture" config user.email test@example.test
  printf '%s\n' "$fixture"
}

run_sync() {
  local fixture="$1"
  shift
  (
    export PATH="$mockbin:$PATH"
    export MOCK_NEW_REV="$new_rev"
    export MOCK_HM_LOG="$hm_log"
    export SKILL_SYNC_REPO="$fixture"
    "$script" --repo "$fixture" "$@"
  )
}

# --help exits 0 and prints usage.
"$script" --help >"$tmp/help.txt"
grep -q '^Usage:' "$tmp/help.txt"
printf 'ok: --help prints usage\n'

# Happy path: update input, validate, commit, push, activate.
fixture="$(make_fixture happy)"
run_sync "$fixture" >/dev/null
assert_eq "$(jq -r '.nodes.skills.locked.rev' "$fixture/flake.lock")" "$new_rev" "happy: lock updated"
assert_eq "$(git -C "$fixture" rev-parse HEAD)" "$(git -C "$fixture" rev-parse origin/master)" "happy: pushed"
git -C "$fixture" log -1 --format=%s | grep -q 'sync skills input'
grep -q 'switch' "$hm_log"
printf 'ok: happy path commits, pushes, and activates\n'

# Second run is a no-op.
before="$(git -C "$fixture" rev-parse HEAD)"
out="$(run_sync "$fixture")"
grep -q 'already current' <<<"$out"
assert_eq "$(git -C "$fixture" rev-parse HEAD)" "$before" "idempotent: no extra commit"

# Dry run changes nothing.
fixture="$(make_fixture dryrun)"
run_sync "$fixture" --dry-run >/dev/null
assert_eq "$(jq -r '.nodes.skills.locked.rev' "$fixture/flake.lock")" "$old_rev" "dry-run: lock untouched"
assert_eq "$(git -C "$fixture" rev-parse HEAD)" "$(git -C "$fixture" rev-parse origin/master)" "dry-run: nothing committed"
assert_eq "$(wc -l <"$hm_log")" "0" "dry-run: no activation"

# Refuses to run off the default branch.
fixture="$(make_fixture offbranch)"
git -C "$fixture" checkout -q -b feature/x
out="$(run_sync "$fixture" 2>&1 >/dev/null || true)"
grep -q "expected 'master'" <<<"$out"
printf 'ok: refuses off the default branch\n'

# Refuses a dirty lockfile.
fixture="$(make_fixture dirtylock)"
printf 'junk\n' >>"$fixture/flake.lock"
out="$(run_sync "$fixture" 2>&1 >/dev/null || true)"
grep -q 'uncommitted changes' <<<"$out"
printf 'ok: refuses a dirty lockfile\n'

# --no-activate skips the switch but still publishes.
fixture="$(make_fixture noactivate)"
run_sync "$fixture" --no-activate >/dev/null
assert_eq "$(jq -r '.nodes.skills.locked.rev' "$fixture/flake.lock")" "$new_rev" "no-activate: lock updated"
assert_eq "$(wc -l <"$hm_log")" "0" "no-activate: no switch"

# --no-push commits locally without pushing.
fixture="$(make_fixture nopush)"
run_sync "$fixture" --no-push >/dev/null
assert_eq "$(git -C "$fixture" rev-parse origin/master)" "$(git -C "$fixture" rev-parse master^)" "no-push: origin untouched"
assert_eq "$(jq -r '.nodes.skills.locked.rev' "$fixture/flake.lock")" "$new_rev" "no-push: lock updated"

printf 'all skill-sync tests passed\n'
