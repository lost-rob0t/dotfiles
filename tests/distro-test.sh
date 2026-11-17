#!/usr/bin/env bash
# End-to-end tests for the distro release-engineering system.
#
# Runs every scenario against a throwaway sandbox checkout (DISTRO_ROOT)
# so the real repository's issues.org / VERSION are never touched:
#   1.  human intake: feat/bug one-liners land in the Inbox
#   2.  triage assigns ISSUE ids and moves entries to Tracked
#   3.  sync compiles issues.org into Prolog facts (example fences ignored)
#   4.  status/next report correctly, priority order respected
#   5.  claim/done/drop transitions + dep facts
#   6.  release gate refuses: open issues, missing evidence, quorum,
#       standing reject, stale votes/attacks
#   7.  evidence records only on real command execution
#   8.  votes are epoch-stamped; council quorum satisfied by distinct agents
#   9.  attack suite green at current epoch satisfies the gate
#   10. release: VERSION bump, archive of done entries, journal reset,
#       issues.org no longer tracks released requirements, changelog written
#   11. next cycle defaults to the following minor milestone
#   12. triage/claim preserve the Tracked section documentation preamble
#
# Usage: tests/distro-test.sh   (requires swipl and python3 on PATH)

set -euo pipefail

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
distro="$repo_root/scripts/distro"

command -v swipl >/dev/null 2>&1 || {
    printf 'tests/distro-test: swipl not found, skipping\n' >&2
    exit 0
}
command -v python3 >/dev/null 2>&1 || {
    printf 'tests/distro-test: python3 not found, skipping\n' >&2
    exit 0
}

pass=0
fail=0

ok() {
    pass=$((pass + 1))
    printf 'ok %d - %s\n' "$pass" "$1"
}

expect_fail() { # desc cmd...
    local desc="$1"; shift
    if "$@" >/dev/null 2>&1; then
        printf 'not ok - %s (command unexpectedly succeeded)\n' "$desc" >&2
        exit 1
    fi
    pass=$((pass + 1))
    printf 'ok %d - %s\n' "$pass" "$desc"
}

expect_output() { # desc expected cmd...
    local desc="$1" expected="$2"; shift 2
    local out
    out="$("$@" 2>/dev/null)"
    if [[ "$out" != *"$expected"* ]]; then
        printf 'not ok - %s\n  expected output containing: %s\n  actual: %s\n' \
            "$desc" "$expected" "$out" >&2
        exit 1
    fi
    pass=$((pass + 1))
    printf 'ok %d - %s\n' "$pass" "$desc"
}

# --- sandbox -----------------------------------------------------------------
sandbox=$(mktemp -d /tmp/opencode/distro-test.XXXXXX)
trap 'rm -rf "$sandbox"' EXIT
mkdir -p "$sandbox/scripts" "$sandbox/distro/engine" "$sandbox/tests" "$sandbox/docs"
cp "$distro" "$sandbox/scripts/distro"
cp -r "$repo_root/distro/engine/." "$sandbox/distro/engine/"
printf '0.1.0\n' > "$sandbox/distro/VERSION"
# Sandbox attack suite: cheap green commands; the real suite is repo policy.
cat > "$sandbox/distro/config.pl" <<'EOF'
council_quorum(2).
attack_step('true').
attack_step('bash -c "exit 0"').
EOF
cat > "$sandbox/issues.org" <<'EOF'
#+title: issues

* Inbox

* Tracked
EOF

run() { DISTRO_ROOT="$sandbox" "$distro" "$@"; }
run_ok() { DISTRO_ROOT="$sandbox" "$distro" "$@" >/dev/null; }

# --- 1. human intake ----------------------------------------------------------
run_ok feat "add a lock screen keybind"
run_ok bug "git-sync races the qtile reload" --severity high
grep -q '^\*\* FEAT: add a lock screen keybind$' "$sandbox/issues.org" \
    && ok "feat one-liner filed to Inbox"
grep -q '^\*\* BUG: git-sync races the qtile reload$' "$sandbox/issues.org" \
    && ok "bug one-liner filed to Inbox"

# --- 2. triage ----------------------------------------------------------------
out=$(run triage)
[[ "$out" == *"ISSUE-0001"* && "$out" == *"ISSUE-0002"* ]] && ok "triage assigns ids"
grep -q 'MILESTONE: 0.2.0' "$sandbox/issues.org" && ok "default milestone is next minor"
[[ ! "$(sed -n '/^\* Inbox/,/^\* Tracked/p' "$sandbox/issues.org")" =~ FEAT ]] \
    && ok "inbox drained after triage"

# --- 3. sync + fences ---------------------------------------------------------
cat >> "$sandbox/issues.org" <<'EOF'

Example doc fence that must never become an issue:

#+begin_example
** ISSUE-9999: not a real issue
#+end_example
EOF
run_ok sync
[[ ! -f "$sandbox/distro/state/facts.pl" ]] || \
    grep -q "ISSUE-9999" "$sandbox/distro/state/facts.pl" && {
        printf 'not ok - example fence leaked into facts\n' >&2
        exit 1
    }
ok "sync ignores #+begin_example fences"

# --- 4. status / next ---------------------------------------------------------
expect_output "status shows open issues" "milestone 0.2.0: 2 open, 0 done" run status
expect_output "next picks high severity first" "ISSUE-0002" run next

# --- 5. transitions and deps ---------------------------------------------------
run_ok claim ISSUE-0002 --agent agent-a
grep -q 'CLAIMED_BY: agent-a' "$sandbox/issues.org" && ok "claim records agent"
run_ok dep ISSUE-0001 ISSUE-0002
expect_output "done without evidence still gates later" "ISSUE-0001 done" run done ISSUE-0001
run_ok verify ISSUE-0001 -- true
out=$(run verify ISSUE-0001 -- false 2>&1) && {
    printf 'not ok - verify accepted a failing command\n' >&2
    exit 1
}
grep -q "evidence('ISSUE-0001', 'false', 1," "$sandbox/distro/state/journal.pl" \
    && ok "failing verify records failing evidence"

# --- 12. tracked section documentation survives rewrites ----------------------
sandbox2=$(mktemp -d /tmp/opencode/distro-test2.XXXXXX)
trap 'rm -rf "$sandbox2"' EXIT
mkdir -p "$sandbox2/scripts" "$sandbox2/distro/engine"
cp "$distro" "$sandbox2/scripts/distro"
cp -r "$repo_root/distro/engine/." "$sandbox2/distro/engine/"
printf '0.1.0\n' > "$sandbox2/distro/VERSION"
cat > "$sandbox2/issues.org" <<'EOF'
#+title: issues

* Inbox

* Tracked

Structured requirement records, written and maintained by =distro=
subcommands and agents.  This documentation block must survive triage
and claim rewrites of the tracked entries.

#+begin_example
** ISSUE-0001: format example, never a real entry
:PROPERTIES:
:STATUS: open
:END:
#+end_example
EOF
run2() { DISTRO_ROOT="$sandbox2" "$distro" "$@"; }
run2_ok() { DISTRO_ROOT="$sandbox2" "$distro" "$@" >/dev/null; }
run2_ok feat "documented preamble survives"
run2_ok triage
run2_ok claim ISSUE-0001 --agent agent-doc
grep -q 'must survive triage' "$sandbox2/issues.org" \
    && grep -q 'CLAIMED_BY: agent-doc' "$sandbox2/issues.org" \
    || {
        printf 'not ok - claim/triage drops the Tracked documentation preamble\n' >&2
        exit 1
    }
ok "claim preserves the Tracked documentation preamble"

# --- 6. gate refusals ----------------------------------------------------------
run_ok done ISSUE-0002
run_ok verify ISSUE-0002 -- true
expect_fail "release refused: no council votes" run release --yes
run_ok vote approve "ship it" --councilor councilor-1
expect_fail "release refused: quorum unmet" run release --yes
run_ok vote reject "attack found a race" --councilor councilor-2
expect_fail "release refused: standing reject" run release --yes
run_ok vote approve "rechecked, fine now" --councilor councilor-2
expect_fail "release refused: attack suite stale" run release --yes

# --- 7. stale votes: issues.org changes after voting ---------------------------
run_ok bug "one more thing"
run_ok triage >/dev/null   # ISSUE-0003 now tracked, epoch moved
out=$(run status 2>&1)
[[ "$out" == *"stale_votes"* ]] && ok "votes go stale when issues.org changes"
# undo: drop the extra issue and the stale state resets to quorum/attack gates
run_ok drop ISSUE-0003 "test helper"
# the drop moved the epoch, so the council re-votes on the final tracked state
run_ok vote approve "re-vote on final state" --councilor councilor-1
run_ok vote approve "re-vote on final state" --councilor councilor-2

# --- 8. attack -----------------------------------------------------------------
run_ok attack
expect_output "attack greens the gate" "attack ok" run attack

# --- 9. release ----------------------------------------------------------------
out=$(run release --yes)
[[ "$out" == *"released v0.2.0"* ]] && ok "release bumps to milestone version"
[[ "$(cat "$sandbox/distro/VERSION")" == "0.2.0" ]] && ok "VERSION updated"
[[ ! "$(cat "$sandbox/issues.org")" =~ ISSUE-0001 ]] \
    && ok "released requirements no longer tracked"
grep -q "ISSUE-0001" "$sandbox/distro/archive/v0.2.0.org" \
    && ok "requirements archived"
grep -q "release('0.2.0', '0.1.0'" "$sandbox/distro/archive/releases.pl" \
    && ok "durable release record kept"
grep -q "v0.2.0" "$sandbox/distro/CHANGELOG.org" && ok "changelog written"
out=$(run status 2>&1)
[[ "$out" == *"no tracked issues"* ]] && ok "fresh cycle after release"

# --- 10. next cycle ------------------------------------------------------------
run_ok feat "next cycle feature"
run_ok triage >/dev/null
expect_output "next milestone follows the release" "MILESTONE: 0.3.0" \
    grep "MILESTONE" "$sandbox/issues.org"

printf '\ndistro-test: %d passed, %d failed\n' "$pass" "$fail"
[[ $fail -eq 0 ]]
