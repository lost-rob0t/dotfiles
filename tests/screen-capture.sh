#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
subject="${SUBJECT:-$repo_root/.local/bin/screen-capture}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
fake="$tmp/bin"
mkdir -p "$fake" "$tmp/home"

export HOME="$tmp/home"
export SCREEN_CAPTURE_SESSION=x11
export SCREEN_CAPTURE_RUNTIME_DIR="$tmp/runtime"
export SCREEN_CAPTURE_SCREENSHOT_DIR="$tmp/shots"
export SCREEN_CAPTURE_VIDEO_DIR="$tmp/videos"
export SCREEN_CAPTURE_TEST_CLIPBOARD="$tmp/clipboard"
export PATH="$fake:/usr/bin:/bin"

cat > "$fake/maim" <<'SH'
#!/usr/bin/env bash
set -e
out="${@: -1}"
printf 'png' > "$out"
SH

cat > "$fake/xclip" <<'SH'
#!/usr/bin/env bash
set -e
file=""
while (( $# )); do
  if [[ "$1" == "-i" && $# -gt 1 ]]; then
    file="$2"
    shift 2
    continue
  fi
  shift
done
if [[ -n "$file" ]]; then
  cat "$file" > "${SCREEN_CAPTURE_TEST_CLIPBOARD:?}"
else
  cat > "${SCREEN_CAPTURE_TEST_CLIPBOARD:?}"
fi
SH

cat > "$fake/slop" <<'SH'
#!/usr/bin/env bash
printf '0x1234\n'
SH

cat > "$fake/ffmpeg" <<'SH'
#!/usr/bin/env bash
set -Eeuo pipefail
out="${@: -1}"
if [[ "${SCREEN_CAPTURE_TEST_EMPTY:-0}" == "1" ]]; then
  exit 0
fi
finish() {
  printf 'video' > "$out"
  exit 0
}
trap finish INT TERM
while :; do sleep 0.05; done
SH

cat > "$fake/notify-send" <<'SH'
#!/usr/bin/env bash
exit 0
SH

chmod +x "$fake"/*

region="$($subject screenshot-region)"
[[ -s "$region" ]]
[[ "$(cat "$SCREEN_CAPTURE_TEST_CLIPBOARD")" == png ]]

full="$($subject screenshot-screen)"
[[ -s "$full" ]]
[[ "$(cat "$SCREEN_CAPTURE_TEST_CLIPBOARD")" == png ]]

empty="$tmp/custom/empty.mp4"
if SCREEN_CAPTURE_TEST_EMPTY=1 "$subject" record-window "$empty"; then
  printf 'screen-capture: empty recorder output unexpectedly succeeded\n' >&2
  exit 1
fi
[[ ! -e "$empty" ]]

video="$tmp/custom/final.mp4"
$subject record-window "$video" > "$tmp/record.out" &
runner=$!
for _ in $(seq 1 100); do
  [[ -f "$SCREEN_CAPTURE_RUNTIME_DIR/recorder.pid" ]] && break
  sleep 0.02
done

[[ "$($subject status)" == recording$'\t'*"$video" ]]
$subject stop
wait "$runner"
[[ -s "$video" ]]
[[ "$(cat "$SCREEN_CAPTURE_TEST_CLIPBOARD")" == "$video" ]]
[[ "$($subject status 2>/dev/null || true)" == idle ]]

printf 'screen-capture: ok\n'
