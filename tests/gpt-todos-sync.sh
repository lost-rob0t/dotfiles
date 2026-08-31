#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
SYNC="$ROOT/scripts/gpt-todos-sync"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

REMOTE="$TMP/remote.git"
SEED="$TMP/seed"
REPO="$TMP/durable"
ORG="$TMP/org"
RUN="$TMP/run"
STATE="$TMP/state"

mkdir -p "$ORG" "$RUN" "$STATE"
git init --bare "$REMOTE" >/dev/null
git clone "$REMOTE" "$SEED" >/dev/null 2>&1
mkdir -p "$SEED/agenda"

printf '* TODO shared\n' > "$SEED/agenda/shared.org"
printf '* TODO remote\n' > "$SEED/agenda/remote.org"
git -C "$SEED" add agenda
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -m seed >/dev/null
git -C "$SEED" push -u origin HEAD >/dev/null 2>&1

run_sync() {
  HOME="$TMP/home" \
  XDG_RUNTIME_DIR="$RUN" \
  XDG_STATE_HOME="$STATE" \
  DOTFILES_DIR="$TMP/no-dotfiles" \
  GPT_TODOS_REPO_DIR="$REPO" \
  GPT_TODOS_ORG_DIR="$ORG" \
  GPT_TODOS_REMOTE="$REMOTE" \
  GIT_AUTHOR_NAME=test \
  GIT_AUTHOR_EMAIL=test@example.invalid \
  GIT_COMMITTER_NAME=test \
  GIT_COMMITTER_EMAIL=test@example.invalid \
    bash "$SYNC" "$@"
}

commit_in_repo() {
  git -C "$REPO" -c user.name=test -c user.email=test@example.invalid \
    commit -q --all -m "$1"
}

subject_of() { git -C "$REPO" log -1 --format=%s; }
body_of()    { git -C "$REPO" log -1 --format=%b; }

# Initial remote state is restored into the complete local agenda tree.
run_sync
cmp "$ORG/shared.org" "$SEED/agenda/shared.org"
cmp "$ORG/remote.org" "$SEED/agenda/remote.org"

# New local agenda files, including nested project files, are first-class sync
# inputs because Emacs discovers the agenda tree recursively.
printf '* TODO local-only\n' > "$ORG/local-only.org"
mkdir -p "$ORG/projects/demo"
printf '* TODO nested-local\n' > "$ORG/projects/demo/tasks.org"
printf '* DONE shared-local\n' > "$ORG/shared.org"
run_sync
git -C "$SEED" pull >/dev/null 2>&1
cmp "$ORG/shared.org" "$SEED/agenda/shared.org"
cmp "$ORG/local-only.org" "$SEED/agenda/local-only.org"
cmp "$ORG/projects/demo/tasks.org" "$SEED/agenda/projects/demo/tasks.org"
git -C "$SEED" ls-files --error-unmatch agenda/local-only.org >/dev/null
git -C "$SEED" ls-files --error-unmatch agenda/projects/demo/tasks.org >/dev/null

# A new remote nested agenda file is restored locally without flattening it.
mkdir -p "$SEED/agenda/projects/remote"
printf '* TODO remote-nested\n' > "$SEED/agenda/projects/remote/tasks.org"
git -C "$SEED" add agenda/projects/remote/tasks.org
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -m remote-nested >/dev/null
git -C "$SEED" push >/dev/null 2>&1
run_sync
grep -q 'remote-nested' "$ORG/projects/remote/tasks.org"

printf '* DONE remote-change\n' > "$SEED/agenda/remote.org"
git -C "$SEED" add agenda/remote.org
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -m remote >/dev/null
git -C "$SEED" push >/dev/null 2>&1
run_sync
grep -q 'remote-change' "$ORG/remote.org"

# Agenda files may be symlinked into the live tree. Saving through that symlink
# dirties the durable checkout itself. --file must snapshot the just-written
# contents, temporarily clear only that owned path, pull, restore the snapshot,
# commit, and push without losing the symlink or touching unrelated paths.
rm -- "$ORG/remote.org"
ln -s "$REPO/agenda/remote.org" "$ORG/remote.org"
printf '* DONE emacs-save\n' > "$ORG/remote.org"
[[ -n "$(git -C "$REPO" status --porcelain -- agenda/remote.org)" ]]
run_sync --file "$ORG/remote.org"
[[ -L "$ORG/remote.org" ]]
git -C "$SEED" pull >/dev/null 2>&1
grep -q 'emacs-save' "$SEED/agenda/remote.org"
grep -q 'emacs-save' "$ORG/remote.org"
[[ -z "$(git -C "$REPO" status --porcelain -- agenda)" ]]

# A true same-file remote conflict must never eat the Emacs save. Save mode
# rewinds the aliased durable path only long enough to inspect/pull. When both
# sides changed from the same baseline, it fails closed, restores the exact
# local save to the symlink target, and leaves a recovery copy in user state.
printf '* DONE remote-conflict-save\n' > "$SEED/agenda/remote.org"
git -C "$SEED" add agenda/remote.org
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -m remote-conflict-save >/dev/null
git -C "$SEED" push >/dev/null 2>&1
printf '* TODO local-conflict-save\n' > "$ORG/remote.org"

set +e
run_sync --file "$ORG/remote.org"
save_conflict_rc=$?
set -e
[[ "$save_conflict_rc" -eq 5 ]]
[[ -L "$ORG/remote.org" ]]
grep -q 'local-conflict-save' "$ORG/remote.org"
grep -q 'local-conflict-save' "$REPO/agenda/remote.org"
find "$STATE/gpt-todos-sync/recovery" -type f -name '*agenda__remote.org' \
  -exec grep -q 'local-conflict-save' {} \; -print | grep -q .

# Put the fixture back on the last committed baseline, then prove a normal run
# can consume the pending remote side after the user resolves/discards local.
git -C "$REPO" restore --worktree --source=HEAD -- agenda/remote.org
run_sync
grep -q 'remote-conflict-save' "$ORG/remote.org"

# Concurrent edits remain fail-closed.
printf '* TODO local-conflict\n' > "$ORG/shared.org"
git -C "$SEED" pull >/dev/null 2>&1
printf '* TODO remote-conflict\n' > "$SEED/agenda/shared.org"
git -C "$SEED" add agenda/shared.org
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -m conflict >/dev/null
git -C "$SEED" push >/dev/null 2>&1

set +e
run_sync
rc=$?
set -e
[[ "$rc" -eq 5 ]]
grep -q 'local-conflict' "$ORG/shared.org"

# Resolve the deliberate conflict above by accepting the remote side, then
# reset to a synchronized baseline so each smart-message scenario below owns
# exactly one commit.
cp "$SEED/agenda/shared.org" "$ORG/shared.org"
run_sync

printf '* TODO shared-reset\n' > "$ORG/shared.org"
run_sync

# Checkbox completions produce a counted subject; statistics-cookie churn
# ([0/3] -> [3/3]) is recognized as derived noise, not a content edit.
printf '* server research\n*** group [0/3]\n- [ ] alpha\n- [ ] beta\n- [ ] gamma\n' \
  > "$ORG/smart.org"
run_sync
[[ "$(subject_of)" == 'agenda: add 1 agenda file' ]]

sed -i -e 's/\[0\/3\]/[3\/3]/' -e 's/- \[ \]/- [X]/' "$ORG/smart.org"
run_sync
[[ "$(subject_of)" == 'agenda: mark 3 done in smart.org' ]]
[[ -z "$(body_of)" ]]

# Mixed done/reopen work across files counts both and lists per-file bullets.
printf '* multi\n- [ ] p1\n- [ ] p2\n' > "$ORG/smart-multi.org"
run_sync
[[ "$(subject_of)" == 'agenda: add 1 agenda file' ]]

sed -i 's/- \[ \] p1/- [X] p1/' "$ORG/smart-multi.org"
sed -i 's/- \[X\] alpha/- [ ] alpha/' "$ORG/smart.org"
run_sync
[[ "$(subject_of)" == 'agenda: mark 1 done, reopen 1 across 2 files' ]]
body_of | grep -q -- '- smart-multi.org: 1 done'
body_of | grep -q -- '- smart.org: 1 reopened'

# TODO -> DONE headline transitions count as done-marking.
printf '* TODO write report\n' > "$ORG/smart-headline.org"
run_sync
[[ "$(subject_of)" == 'agenda: add 1 agenda file' ]]

printf '* DONE write report\n' > "$ORG/smart-headline.org"
run_sync
[[ "$(subject_of)" == 'agenda: mark 1 done in smart-headline.org' ]]

# Edits with no task-state signal still sync under a counted update subject.
printf 'some new context line\n' >> "$ORG/smart-headline.org"
run_sync
[[ "$(subject_of)" == 'agenda: update 1 agenda file' ]]

printf 'more context\n' >> "$ORG/smart-headline.org"
printf '* TODO noise\nplain edit\n' > "$ORG/smart-noise.org"
run_sync
[[ "$(subject_of)" == 'agenda: add 1, update 1 agenda files' ]]

# A local commit whose push failed must not wedge the sync: the next run
# publishes it instead of failing 'Not possible to fast-forward' forever.
printf '* TODO heal\n' > "$ORG/heal.org"
run_sync

printf '* TODO heal-stranded\n' > "$REPO/agenda/heal.org"
commit_in_repo stranded
run_sync
git -C "$SEED" pull >/dev/null 2>&1
git -C "$SEED" log --format=%s -3 | grep -q '^stranded$'

# Divergence with non-overlapping changes is healed by replaying the unpushed
# commits onto upstream and pushing; remote progress still flows to the agenda.
printf '* TODO local-diverge\n' > "$REPO/agenda/heal.org"
commit_in_repo local-diverge
printf '* TODO remote-diverge\n' > "$SEED/agenda/remote.org"
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -q --all -m remote-diverge
git -C "$SEED" push >/dev/null 2>&1
run_sync
git -C "$SEED" pull >/dev/null 2>&1
git -C "$SEED" log --format=%s -3 | grep -q '^local-diverge$'
grep -q 'remote-diverge' "$ORG/remote.org"
grep -q 'local-diverge' "$ORG/heal.org"

# Divergence that cannot replay cleanly fails closed, keeps the unpushed
# commits, and leaves no rebase in progress.
printf '* TODO local-clash\n' > "$REPO/agenda/heal.org"
commit_in_repo local-clash
printf '* TODO remote-clash\n' > "$SEED/agenda/heal.org"
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -q --all -m remote-clash
git -C "$SEED" push >/dev/null 2>&1

set +e
run_sync
heal_conflict_rc=$?
set -e
[[ "$heal_conflict_rc" -ne 0 ]]
[[ "$(git -C "$REPO" log -1 --format=%s)" == 'local-clash' ]]
[[ -z "$(git -C "$REPO" status --porcelain -- agenda)" ]]
grep -q 'local-diverge' "$ORG/heal.org"

printf 'gpt-todos recursive agenda sync tests passed\n'
