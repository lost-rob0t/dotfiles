#!/usr/bin/env bash
# Contract tests for the pi rlm-ops extension: Prolog drivers, TypeScript
# modules, and extension load. Runs everywhere with swipl + node >= 24;
# the pi load smoke is skipped when pi is not installed.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
ext="$here/../pi/agent/extensions/rlm-ops"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }

command -v swipl >/dev/null || fail "swipl required"
command -v node >/dev/null || fail "node required"

# ---------------------------------------------------------------- drivers

kb() {  # kb <op> <args...>
	KB_ARGS=$(printf '%s' "$1" | python3 -c 'import json,sys;print(json.dumps(sys.stdin.read().split(" ")))') \
		flock -x "$work/board.pl.lock" swipl -q "$ext/kb_driver.pl"
}

echo "== kb_driver contract =="
out=$(KB_ARGS='["ping"]' swipl -q "$ext/kb_driver.pl")
grep -q '"ok":true' <<<"$out" || fail "ping: $out"

board="$work/board.pl"
kb "post $board main agent-1 task go(1)" >/dev/null
kb "post $board agent-1 main result done(2)" >/dev/null
kb "assert $board state(running)" >/dev/null

out=$(KB_ARGS=$(python3 -c 'import json;print(json.dumps(["query","'"$board"'","msg(F,T,K,P,S,_)","10"]))') \
	flock -x "$board.lock" swipl -q "$ext/kb_driver.pl")
grep -q '"count":2' <<<"$out" || fail "query expects 2 messages: $out"
grep -q "go(1)" <<<"$out" || fail "query lost payload: $out"

out=$(kb "take $board agent-1")
grep -q '"count":1' <<<"$out" || fail "take expects 1 message: $out"
out=$(kb "take $board agent-1")
grep -q '"count":0' <<<"$out" || fail "mailbox must drain atomically: $out"

out=$(kb "retract $board state(_)")
grep -q '"retracted":1' <<<"$out" || fail "retract: $out"

if KB_ARGS='["parse","foo(("]' swipl -q "$ext/kb_driver.pl" >/dev/null 2>&1; then
	fail "parse must reject invalid terms"
fi
out=$(KB_ARGS=$(python3 -c 'import json;print(json.dumps(["assert","'"$board"'","evil(X) :- shell(X)"]))') \
	flock -x "$board.lock" swipl -q "$ext/kb_driver.pl" || true)
grep -q 'kb_fact_expected' <<<"$out" || fail "assert must reject rules/directives: $out"

echo "kb_driver ok"

echo "== gate_driver contract =="
cat >"$work/obs.pl" <<'EOF'
obs(1, tool(bash), summary('bash pytest -q'), ok(true), stamp(1)).
obs(2, tool(bash), summary('bash make install'), ok(false), stamp(2)).
EOF
cat >"$work/verify-fail.pl" <<'EOF'
tests_green :- obs(_, tool(bash), summary(S), ok(true), _), sub_atom(S, _, _, _, pytest).
install_ok :- obs(_, tool(bash), summary(S), ok(true), _), sub_atom(S, _, _, _, install).
requirement(tests, tests_green).
requirement(install, install_ok).
EOF
cat >"$work/verify-pass.pl" <<'EOF'
tests_green :- obs(_, tool(bash), summary(S), ok(true), _), sub_atom(S, _, _, _, pytest).
requirement(tests, tests_green).
EOF
printf ':- shell("touch %s/gate_pwned").\nrequirement(x, true).\n' "$work" >"$work/verify-evil.pl"

if GATE_ARGS="[
	\"$work/obs.pl\",
	\"$work/verify-fail.pl\"
]" swipl -q "$ext/gate_driver.pl" >"$work/gate-fail.out" 2>/dev/null; then
	fail "gate must exit 1 when a requirement is unproven"
fi
grep -q '"ok":false' "$work/gate-fail.out" || fail "gate fail report: $(cat "$work/gate-fail.out")"
grep -q 'install' "$work/gate-fail.out" || fail "gate must name the failed requirement"

GATE_ARGS="[
	\"$work/obs.pl\",
	\"$work/verify-pass.pl\"
]" swipl -q "$ext/gate_driver.pl" >"$work/gate-pass.out" || fail "gate must pass proven requirements"
grep -q '"ok":true' "$work/gate-pass.out" || fail "gate pass report: $(cat "$work/gate-pass.out")"

if GATE_ARGS="[
	\"$work/obs.pl\",
	\"$work/verify-evil.pl\"
]" swipl -q "$ext/gate_driver.pl" >"$work/gate-evil.out" 2>/dev/null; then
	fail "gate must reject directives fail-closed"
fi
[ ! -e "$work/gate_pwned" ] || fail "directive payload executed"
grep -q 'verify_directive_not_allowed' "$work/gate-evil.out" || fail "gate evil report: $(cat "$work/gate-evil.out")"

echo "gate_driver ok"

# ---------------------------------------------------------------- modules

echo "== module tests =="
node "$here/pi-rlm-ops-modules.mjs" || fail "module tests"
echo "modules ok"

# ------------------------------------------------------------ load smoke

if command -v pi >/dev/null 2>&1; then
	echo "== pi load smoke =="
	pi --extension "$ext" --help 2>&1 | grep -q -- '--symbolic' || fail "extension flag not registered"
	echo "pi load ok"
else
	echo "== pi load smoke skipped (pi not installed) =="
fi

echo "ALL RLM-OPS TESTS PASSED"
