# shellcheck shell=bash
set -euo pipefail

real_opencode="${OPENCODE_REAL_BIN:?OPENCODE_REAL_BIN must point to the real opencode binary}"

# Normal OpenCode behavior is untouched.  Only the explicit wrapper flag opts
# into the StarIntel V4 tmpfs/Prolog-RLM environment.
if [[ "${1:-}" != "--yolo" ]]; then
  exec "$real_opencode" "$@"
fi
shift

workspace="${OPENCODE_YOLO_WORKSPACE:-$HOME/Documents/Projects/starintelV4}"
keep_context="${OPENCODE_KEEP_PROLOG_CONTEXT:-0}"
bootstrap=1
forward=()

usage() {
  cat <<'EOF'
usage: opencode --yolo [--workspace PATH] [--keep-context] [--no-bootstrap] [opencode args...]

Run OpenCode with --auto from the StarIntel V4 workspace while exposing a
project-visible Prolog/spec context backed by user-writable tmpfs.  No root,
mount(8), sudo, or disk fallback is used.  Explicit OpenCode deny rules still
win over --auto.
EOF
}

while (($#)); do
  case "$1" in
    --workspace)
      (($# >= 2)) || { echo "opencode --yolo: --workspace requires a path" >&2; exit 2; }
      workspace="$2"
      shift 2
      ;;
    --workspace=*)
      workspace="${1#*=}"
      shift
      ;;
    --keep-context)
      keep_context=1
      shift
      ;;
    --no-bootstrap)
      bootstrap=0
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      forward+=("$1")
      shift
      ;;
  esac
done

workspace="$(realpath -e -- "$workspace")" || {
  echo "opencode --yolo: workspace does not exist: $workspace" >&2
  exit 1
}
[[ -d "$workspace" ]] || {
  echo "opencode --yolo: workspace is not a directory: $workspace" >&2
  exit 1
}

is_user_tmpfs() {
  local path="$1"
  [[ -n "$path" && -d "$path" && -w "$path" ]] || return 1
  [[ "$(stat -f -c '%T' -- "$path" 2>/dev/null || true)" == "tmpfs" ]] || return 1
  # Do not depend on root-owned setup beyond the ordinary shared /dev/shm
  # tmpfs.  The actual context directory is always created by this user and
  # chmod 0700 below.
  return 0
}

choose_tmpfs_root() {
  local candidate

  if [[ -n "${OPENCODE_TMPFS_ROOT:-}" ]]; then
    if is_user_tmpfs "$OPENCODE_TMPFS_ROOT"; then
      realpath -e -- "$OPENCODE_TMPFS_ROOT"
      return 0
    fi
    echo "opencode --yolo: OPENCODE_TMPFS_ROOT is not a writable tmpfs: $OPENCODE_TMPFS_ROOT" >&2
    return 1
  fi

  # XDG_RUNTIME_DIR is normally /run/user/$UID and is the preferred private,
  # user-owned tmpfs.  /dev/shm is the portable non-root fallback.
  for candidate in "${XDG_RUNTIME_DIR:-}" /dev/shm; do
    [[ -n "$candidate" ]] || continue
    if is_user_tmpfs "$candidate"; then
      realpath -e -- "$candidate"
      return 0
    fi
  done

  echo "opencode --yolo: no writable tmpfs found (checked XDG_RUNTIME_DIR and /dev/shm)" >&2
  exit 1
}

tmpfs_root="$(choose_tmpfs_root)"
context_real="$(mktemp -d "$tmpfs_root/prolog-tmp-spec-context.$(id -u).XXXXXXXX")"
chmod 700 "$context_real"
mkdir -p "$context_real/spec" "$context_real/tmp"

context_view="$workspace/prolog-tmp-spec-context"
if [[ -L "$context_view" && ! -e "$context_view" ]]; then
  rm -- "$context_view"
fi
if [[ -e "$context_view" || -L "$context_view" ]]; then
  echo "opencode --yolo: refusing to replace existing context path: $context_view" >&2
  rm -rf -- "$context_real"
  exit 1
fi
ln -s -- "$context_real" "$context_view"

cat >"$context_real/context.prolog" <<'PROLOG'
% Ephemeral Prolog-RLM context for an OpenCode --yolo session.
% Keep this KB compact, provenance-aware, and tied to exact repository state.
:- dynamic requirement/1.
:- dynamic hypothesis/1.
:- dynamic needs_verification/1.
:- dynamic observed_fact/4.
:- dynamic decision/1.
:- dynamic decision_reason/2.
:- dynamic todo/1.
:- dynamic completed/1.
:- dynamic test_result/3.
:- dynamic proof_result/3.
PROLOG

cleanup() {
  local status=$?
  if [[ -L "$context_view" ]]; then
    local target
    target="$(readlink -f -- "$context_view" 2>/dev/null || true)"
    [[ "$target" != "$context_real" ]] || rm -f -- "$context_view"
  fi
  if [[ "$keep_context" == "1" ]]; then
    printf 'opencode --yolo: kept tmpfs context at %s\n' "$context_real" >&2
  else
    rm -rf -- "$context_real"
  fi
  return "$status"
}
trap cleanup EXIT

export OPENCODE_YOLO=1
export OPENCODE_YOLO_WORKSPACE="$workspace"
export STARINTEL_V4_ROOT="$workspace"
export PROLOG_TMP_SPEC_CONTEXT="$context_view"
export PROLOG_TMP_SPEC_CONTEXT_REAL="$context_real"
export TMPDIR="$context_real/tmp"

spec_prompt="$workspace/spec.prompt.org"
if [[ -f "$spec_prompt" ]]; then
  export STARINTEL_SPEC_PROMPT="$spec_prompt"
else
  unset STARINTEL_SPEC_PROMPT 2>/dev/null || true
fi

has_auto=0
has_prompt=0
for arg in "${forward[@]}"; do
  [[ "$arg" == "--auto" ]] && has_auto=1
  [[ "$arg" == "--prompt" || "$arg" == --prompt=* ]] && has_prompt=1
done

args=()
(( has_auto )) || args+=(--auto)

if (( bootstrap && ! has_prompt )); then
  if [[ -f "$spec_prompt" ]]; then
    bootstrap_prompt="Read $spec_prompt completely before implementation. Load /spec, prolog-reasoning, and every applicable starintel* skill. Use $context_view/context.prolog as the live Prolog-RLM context: query what is known, identify missing facts, use the smallest repository/tool/subagent to verify them, assert compact facts with provenance and exact source SHA, then query again. Put scratch specs and symbolic working state only under $context_view; keep durable source changes in the workspace/repositories. Verify the final implementation against the Prolog specification before declaring success."
  else
    bootstrap_prompt="A user-writable tmpfs Prolog/spec context is available at $context_view. Load /spec, prolog-reasoning, and applicable starintel* skills when relevant. Use $context_view/context.prolog as compact Prolog-RLM working state with provenance; never commit this ephemeral context."
  fi
  args+=(--prompt "$bootstrap_prompt")
fi

args+=("${forward[@]}")
cd "$workspace"
printf 'opencode --yolo: workspace=%s\n' "$workspace" >&2
printf 'opencode --yolo: tmpfs=%s -> %s\n' "$context_view" "$context_real" >&2
"$real_opencode" "${args[@]}"
