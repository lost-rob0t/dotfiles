#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
wrapper="$root/nix/home-manager/files/opencode-yolo.sh"

if [[ ! -d /dev/shm || ! -w /dev/shm ]]; then
  echo "test requires writable /dev/shm" >&2
  exit 1
fi
if [[ "$(stat -f -c '%T' /dev/shm)" != "tmpfs" ]]; then
  echo "/dev/shm is not tmpfs" >&2
  exit 1
fi

case_root="$(mktemp -d)"
cleanup() {
  rm -rf -- "$case_root"
}
trap cleanup EXIT

workspace="$case_root/starintelV4"
mkdir -p "$workspace"
printf '* Runtime specification\n' >"$workspace/spec.prompt.org"

capture="$case_root/capture"
fake="$case_root/opencode-real"
cat >"$fake" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
{
  printf 'ARGS=%s\n' "$*"
  printf 'PWD=%s\n' "$PWD"
  printf 'CTX=%s\n' "${PROLOG_TMP_SPEC_CONTEXT:-}"
  printf 'REAL=%s\n' "${PROLOG_TMP_SPEC_CONTEXT_REAL:-}"
  printf 'SPEC=%s\n' "${STARINTEL_SPEC_PROMPT:-}"
  if [[ -n "${PROLOG_TMP_SPEC_CONTEXT:-}" ]]; then
    printf 'FS=%s\n' "$(stat -f -c '%T' "$PROLOG_TMP_SPEC_CONTEXT_REAL")"
    test -f "$PROLOG_TMP_SPEC_CONTEXT/context.prolog"
    test -L "$STARINTEL_V4_ROOT/prolog-tmp-spec-context"
  fi
} >"$CAPTURE"
EOF
chmod +x "$fake"

# Ordinary opencode use passes straight through: no StarIntel context and no
# implicit --auto.
CAPTURE="$capture" OPENCODE_REAL_BIN="$fake" \
  bash "$wrapper" run hello

grep -Fq 'ARGS=run hello' "$capture"
grep -Fq 'CTX=' "$capture"
if grep -Fq -- '--auto' "$capture"; then
  echo "ordinary opencode invocation unexpectedly enabled --auto" >&2
  exit 1
fi

# --yolo selects the V4 workspace and creates only user-owned state in tmpfs.
CAPTURE="$capture" \
OPENCODE_REAL_BIN="$fake" \
OPENCODE_YOLO_WORKSPACE="$workspace" \
OPENCODE_TMPFS_ROOT=/dev/shm \
  bash "$wrapper" --yolo --model test/model

grep -Fq -- '--auto' "$capture"
grep -Fq -- '--model test/model' "$capture"
grep -Fq -- '--prompt ' "$capture"
grep -Fq "PWD=$workspace" "$capture"
grep -Fq "CTX=$workspace/prolog-tmp-spec-context" "$capture"
grep -Fq "SPEC=$workspace/spec.prompt.org" "$capture"
grep -Fq 'FS=tmpfs' "$capture"

test ! -e "$workspace/prolog-tmp-spec-context"
real="$(sed -n 's/^REAL=//p' "$capture")"
test -n "$real"
test ! -e "$real"

echo "opencode --yolo tmpfs wrapper: ok"
