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

run_sync
cmp "$ORG/shared.org" "$SEED/agenda/shared.org"
cmp "$ORG/remote.org" "$SEED/agenda/remote.org"

printf '* TODO private-local-only\n' > "$ORG/private.org"
printf '* DONE shared-local\n' > "$ORG/shared.org"
run_sync
git -C "$SEED" pull >/dev/null 2>&1
cmp "$ORG/shared.org" "$SEED/agenda/shared.org"
if git -C "$SEED" ls-files --error-unmatch agenda/private.org >/dev/null 2>&1; then
  printf 'untracked local agenda file leaked into gpt-todos\n' >&2
  exit 1
fi

printf '* DONE remote-change\n' > "$SEED/agenda/remote.org"
git -C "$SEED" add agenda/remote.org
git -C "$SEED" -c user.name=test -c user.email=test@example.invalid \
  commit -m remote >/dev/null
git -C "$SEED" push >/dev/null 2>&1
run_sync
grep -q 'remote-change' "$ORG/remote.org"

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

printf 'gpt-todos bidirectional sync tests passed\n'
