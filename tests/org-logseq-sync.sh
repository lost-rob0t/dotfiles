#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
sync_script="$repo_root/scripts/org-logseq-sync"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

mkdir -p \
    "$tmp/bin" \
    "$tmp/roam/daily" \
    "$tmp/logseq/pages" \
    "$tmp/logseq/journals"

printf '* Alpha\n' >"$tmp/roam/alpha.org"
printf 'not a note\n' >"$tmp/roam/ignore.txt"
printf '* Today\n' >"$tmp/roam/daily/2026-09-16.org"
printf '* Beta\n' >"$tmp/logseq/pages/beta.org"
printf '* Yesterday\n' >"$tmp/logseq/journals/2026-09-15.org"

cat >"$tmp/bin/unison" <<'EOF'
#!/usr/bin/env bash
printf '%s\0' "$@" >>"$UNISON_LOG"
printf '\n' >>"$UNISON_LOG"
EOF

cat >"$tmp/bin/emacsclient" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$EMACSCLIENT_LOG"
EOF

chmod +x "$tmp/bin/unison" "$tmp/bin/emacsclient"

export PATH="$tmp/bin:$PATH"
export HOME="$tmp/home"
export XDG_STATE_HOME="$tmp/state"
export ORG_ROAM_DIR="$tmp/roam"
export ORG_ROAM_DAILIES_DIR="daily"
export LOGSEQ_GRAPH_DIR="$tmp/logseq"
export UNISON_LOG="$tmp/unison.log"
export EMACSCLIENT_LOG="$tmp/emacsclient.log"

bash "$sync_script" --once

python3 - "$UNISON_LOG" <<'PY'
import sys
from pathlib import Path

raw = Path(sys.argv[1]).read_bytes()
calls = [chunk for chunk in raw.split(b"\n") if chunk]

if len(calls) != 2:
    raise SystemExit(f"expected two unison calls, got {len(calls)}")

decoded = [
    [part.decode() for part in call.split(b"\0") if part]
    for call in calls
]

notes, journals = decoded

def paths(call):
    return [call[index + 1] for index, value in enumerate(call[:-1]) if value == "-path"]

if notes[0].endswith("/roam") is False or notes[1].endswith("/logseq/pages") is False:
    raise SystemExit(f"unexpected notes roots: {notes[:2]}")

if journals[0].endswith("/roam/daily") is False or journals[1].endswith("/logseq/journals") is False:
    raise SystemExit(f"unexpected journal roots: {journals[:2]}")

if set(paths(notes)) != {"alpha.org", "beta.org"}:
    raise SystemExit(f"unexpected notes paths: {paths(notes)}")

if set(paths(journals)) != {"2026-09-15.org", "2026-09-16.org"}:
    raise SystemExit(f"unexpected journal paths: {paths(journals)}")

if any("ignore.txt" in value for value in notes + journals):
    raise SystemExit("non-Org file leaked into synchronization")
PY

grep -q "org-roam-db-sync" "$EMACSCLIENT_LOG"

printf 'org-logseq-sync tests: OK\n'
