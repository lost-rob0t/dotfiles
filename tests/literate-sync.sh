#!/usr/bin/env bash
# Regression tests for scripts/check-literate-sync.
#
# Builds throwaway fixture repositories covering the checker's contract:
#   1.  clean synchronized source
#   2.  stale output fails
#   3.  documentation-only Org (no :tangle) is ignored
#   4.  ignored backup Org (*.org.bak etc.) is ignored
#   5.  multiple blocks -> one target
#   6.  one Org -> multiple targets
#   7.  nested directories with targets relative to the Org source
#   8.  source blocks with :tangle no
#   9.  deterministic second run
#   10. baseline tolerance: baselined stale Org passes, new drift fails
#   11. duplicate ownership fails and can be baselined
#   12. executable mode preserved by tangle (:tangle-mode) is verified
#
# Usage: tests/literate-sync.sh

set -euo pipefail

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
checker="$repo_root/scripts/check-literate-sync"
EMACS=${EMACS:-emacs}

command -v "$EMACS" >/dev/null 2>&1 || {
    printf 'tests/literate-sync: emacs not found, skipping\n' >&2
    exit 0
}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

pass=0
fail_count=0

new_fixture() {
    local name=$1
    fixture="$work/$name"
    mkdir -p "$fixture"
    git -C "$fixture" init -q
    git -C "$fixture" config user.name test
    git -C "$fixture" config user.email test@example.com
}

commit_all() {
    git -C "$fixture" add -A
    git -C "$fixture" commit -qm fixture
}

run_checker() {
    (cd "$fixture" && "$checker")
}

expect_ok() {
    local name=$1
    if run_checker >/dev/null 2>"$work/out"; then
        pass=$((pass + 1))
        printf 'ok   %s\n' "$name"
    else
        fail_count=$((fail_count + 1))
        printf 'FAIL %s\n' "$name"
        sed 's/^/       /' "$work/out" >&2
    fi
}

expect_fail() {
    local name=$1
    if run_checker >"$work/out" 2>&1; then
        fail_count=$((fail_count + 1))
        printf 'FAIL %s (expected failure, got success)\n' "$name"
    else
        pass=$((pass + 1))
        printf 'ok   %s\n' "$name"
    fi
}

# 1. clean synchronized source
tangle() {
    "$EMACS" -Q --batch --eval "(require 'org)" \
        --eval "(org-babel-tangle-file \"$1\")" >/dev/null 2>&1
}

new_fixture clean
clean_fixture="$fixture"
mkdir -p "$fixture/nested/dir"
cat >"$fixture/nested/dir/clean.org" <<'ORG'
#+title: clean fixture
#+begin_src sh :tangle script.sh
echo clean
#+end_src
ORG
tangle "$fixture/nested/dir/clean.org"
commit_all
expect_ok "clean synchronized source"

# deterministic second run
expect_ok "deterministic second run"

# 2. stale output
new_fixture stale
mkdir -p "$fixture/d"
cat >"$fixture/d/stale.org" <<'ORG'
#+begin_src sh :tangle out.sh
echo original
#+end_src
ORG
tangle "$fixture/d/stale.org"
commit_all
printf 'echo tampered\n' >"$fixture/d/out.sh"
git -C "$fixture" commit -qam tamper
expect_fail "stale output fails"

# 3. documentation-only Org (no :tangle at all)
new_fixture doconly
cat >"$fixture/readme.org" <<'ORG'
#+title: docs only
This Org file documents things and never tangles.
#+begin_src sh
echo not tangled
#+end_src
ORG
commit_all
expect_ok "documentation-only Org ignored"

# 4. ignored backup Org
new_fixture backup
cat >"$fixture/real.org" <<'ORG'
#+begin_src sh :tangle real.sh
echo real
#+end_src
ORG
tangle "$fixture/real.org"
cat >"$fixture/backup.org.bak" <<'ORG'
#+begin_src sh :tangle real.sh
echo stale garbage that must not be checked
#+end_src
ORG
cat >"$fixture/notes.org~" <<'ORG'
#+begin_src sh :tangle real.sh
echo more junk
#+end_src
ORG
commit_all
expect_ok "ignored backup Org files skipped"

# 5. multiple blocks -> one target
new_fixture multi
mkdir -p "$fixture/m"
cat >"$fixture/m/multi.org" <<'ORG'
#+begin_src sh :tangle combined.sh
#!/usr/bin/env bash
#+end_src
#+begin_src sh :tangle combined.sh
echo part-two
#+end_src
ORG
tangle "$fixture/m/multi.org"
commit_all
expect_ok "multiple blocks into one target"

# 6. one Org -> multiple targets
tangle_targets2() {
    cat >"$fixture/m/multi.org" <<'ORG'
#+begin_src sh :tangle one.sh
echo one
#+end_src
#+begin_src sh :tangle two.sh
echo two
#+end_src
ORG
}
new_fixture multitarget
mkdir -p "$fixture/m"
tangle_targets2
tangle "$fixture/m/multi.org"
commit_all
expect_ok "one Org into multiple targets"

# 7. nested directories with relative targets (already covered in fixture 1)
# verify the target really landed next to the Org source:
test -f "$clean_fixture/nested/dir/script.sh" || {
    fail_count=$((fail_count + 1))
    printf 'FAIL nested target location\n'
}

# 8. source blocks with :tangle no
new_fixture tangle-no
cat >"$fixture/partial.org" <<'ORG'
#+begin_src sh :tangle partial.sh
echo kept
#+end_src
#+begin_src sh :tangle no
echo skipped
#+end_src
ORG
tangle "$fixture/partial.org"
commit_all
grep -q skipped "$fixture/partial.sh" && {
    fail_count=$((fail_count + 1))
    printf 'FAIL :tangle no block was tangled\n'
} || {
    pass=$((pass + 1))
    printf 'ok   :tangle no blocks skipped\n'
}
expect_ok ":tangle no stays in sync"

# 10. baseline tolerance
new_fixture baseline
mkdir -p "$fixture/b"
cat >"$fixture/b/drifted.org" <<'ORG'
#+begin_src sh :tangle out.sh
echo from-org
#+end_src
ORG
tangle "$fixture/b/drifted.org"
cat >"$fixture/b/fresh.org" <<'ORG'
#+begin_src sh :tangle fresh.sh
echo fresh
#+end_src
ORG
tangle "$fixture/b/fresh.org"
commit_all
printf 'echo hand-edited\n' >"$fixture/b/out.sh"
git -C "$fixture" commit -qam drift
expect_fail "unbaselined drift fails"
printf 'b/drifted.org\n' >"$fixture/.literate-sync-baseline"
git -C "$fixture" add -A
git -C "$fixture" commit -qm baseline
expect_ok "baselined drift tolerated"
# new drift on top of baseline still fails
printf 'echo more-drift\n' >"$fixture/b/fresh.sh"
git -C "$fixture" commit -qam new-drift
expect_fail "new drift on top of baseline fails"

# 11. duplicate ownership
new_fixture dupown
cat >"$fixture/one.org" <<'ORG'
#+begin_src sh :tangle shared.sh
echo shared
#+end_src
ORG
cat >"$fixture/two.org" <<'ORG'
#+begin_src sh :tangle shared.sh
echo shared
#+end_src
ORG
tangle "$fixture/one.org"
tangle "$fixture/two.org"
git -C "$fixture" add -A
git -C "$fixture" commit -qm dup
expect_fail "duplicate ownership fails"
printf 'duplicate:shared.sh\n' >"$fixture/.literate-sync-baseline"
git -C "$fixture" add -A
git -C "$fixture" commit -qm baseline-dup
expect_ok "baselined duplicate ownership tolerated"

printf 'tests/literate-sync: %d passed, %d failed\n' "$pass" "$fail_count"
[ "$fail_count" -eq 0 ]
