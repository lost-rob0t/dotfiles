#!/usr/bin/env bash
set -Eeuo pipefail

ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
TMP=$(mktemp -d)
trap 'rm -rf -- "$TMP"' EXIT

export HOME="$TMP/home"
export XDG_CACHE_HOME="$TMP/cache"
export KB_ROAM_ROOT="$TMP/notes"
export KB_ROAM_PUBLISH_ROOT="$TMP/site"
export KB_ROAM_ELISP="$ROOT/lisp/wiki/kb-roam.el"
export KB_ROAM_CENSOR_PL="$ROOT/lisp/wiki/roam-censor.pl"
export KB_ROAM_ASSET_DIR="$ROOT/lisp/wiki/assets"
export ROAM_CENSOR_CMD="$ROOT/scripts/roam-censor"
mkdir -p "$HOME" "$KB_ROAM_ROOT/daily"

note=$(bash "$ROOT/scripts/kb-ingest" \
  --title "Prolog Org-roam ingestion" \
  --tags "prolog,org_roam,opencode" \
  --body "Prolog policy protects Org-roam ingestion from OpenCode." \
  --source "test:kb-roam" \
  --ai)

grep -q '^#+ROAM_VISIBILITY: private$' "$note"
grep -q '^#+AI_GENERATED: t$' "$note"
ai_tag=$(sed -n 's/^#+AI_TAG: //p' "$note")
[[ "$ai_tag" =~ ^ai_[a-z0-9]{16}$ ]]
grep -q ":$ai_tag:" "$note"

cat >"$KB_ROAM_ROOT/daily/2026-09-20.org" <<'ORG'
:PROPERTIES:
:ID: daily-test
:END:
#+TITLE: Public daily fixture
#+ROAM_SCHEMA: org-roam-meta/v1
#+ROAM_KIND: daily
#+ROAM_VISIBILITY: public
#+CENSOR_PROFILE: strict
#+FILETAGS: :org_roam:publishing:

* Public
PUBLIC-SENTINEL

* Secret :private:
PRIVATE-SENTINEL

* Property secret
:PROPERTIES:
:PRIVATE: t
:END:
PRIVATE-PROPERTY-SENTINEL
ORG

bash "$ROOT/scripts/roam-censor" check
bash "$ROOT/scripts/roam-publish"

grep -R -q 'PUBLIC-SENTINEL' "$KB_ROAM_PUBLISH_ROOT"
! grep -R -q 'PRIVATE-SENTINEL' "$KB_ROAM_PUBLISH_ROOT"
! grep -R -q 'PRIVATE-PROPERTY-SENTINEL' "$KB_ROAM_PUBLISH_ROOT"
test -f "$KB_ROAM_PUBLISH_ROOT/assets/roam.css"
test -f "$KB_ROAM_PUBLISH_ROOT/daily/index.html"

cat >"$KB_ROAM_ROOT/bad-ai.org" <<'ORG'
#+TITLE: Bad AI
#+ROAM_SCHEMA: org-roam-meta/v1
#+ROAM_KIND: note
#+ROAM_VISIBILITY: public
#+CENSOR_PROFILE: strict
#+AI_GENERATED: t
#+FILETAGS: :prolog:testing:
ORG

if bash "$ROOT/scripts/roam-censor" check >/dev/null 2>&1; then
  echo "expected missing AI tag to fail" >&2
  exit 1
fi
rm "$KB_ROAM_ROOT/bad-ai.org"

mkdir -p "$TMP/bin"
cat >"$TMP/bin/opencode" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
if [[ "$1 $2" == "session list" ]]; then
  printf '[{"id":"ses_test"}]\n'
elif [[ "$1 $2" == "session export" ]]; then
  printf '{"id":"ses_test","messages":[{"role":"assistant","text":"hello"}]}\n'
else
  exit 2
fi
SH
chmod +x "$TMP/bin/opencode"

PATH="$TMP/bin:$PATH" bash "$ROOT/scripts/opencode-session-to-org" ses_test >/dev/null
session_file=$(grep -Rl '^#+AI_SESSION_ID: ses_test$' "$KB_ROAM_ROOT/ai/sessions")
grep -q '^#+ROAM_VISIBILITY: private$' "$session_file"
grep -q ':opencode:session:' "$session_file"

echo "kb-roam tests: OK"
