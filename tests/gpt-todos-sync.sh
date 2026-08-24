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

mkdir -p "$ORG" "$RUN"
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
  DOTFILES_DIR="$TMP/no-dotfiles" \
  GPT_TODOS_REPO_DIR="$REPO" \
  GPT_TODOS_ORG_DIR="$ORG" \
  GPT_TODOS_REMOTE="$REMOTE" \
  GIT_AUTHOR_NAME=test \
  GIT_AUTHOR_EMAIL=test@example.invalid \
  GIT_COMMITTER_NAME=test \
  GIT_COMMITTER_EMAIL=test@example.invalid \
    bash "$SYNC"
}

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

# Agenda files may be symlinked into the live tree. If a live path resolves to
# the durable file itself, deployment must treat it as already synchronized
# instead of asking cp to copy a file onto the same inode.
rm -- "$ORG/remote.org"
ln -s "$REPO/agenda/remote.org" "$ORG/remote.org"
run_sync
[[ -L "$ORG/remote.org" ]]
grep -q 'remote-change' "$ORG/remote.org"

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

printf 'gpt-todos recursive agenda sync tests passed\n'
