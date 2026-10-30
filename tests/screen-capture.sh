#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
subject="${SUBJECT:-$repo_root/.local/bin/screen-capture}"
timeout_bin="$(command -v timeout)"
qtile_capture_org="$repo_root/.config/qtile/qtile-capture.org"
qtile_capture_py="$repo_root/.config/qtile/qtile_capture.py"
qtile_docs="$repo_root/.config/qtile/qtile.org"
tmp="$(mktemp -d)"
fake="$tmp/bin"
mkdir -p "$fake" "$tmp/home"
runner=

export HOME="$tmp/home"
export SCREEN_CAPTURE_SESSION=x11
export SCREEN_CAPTURE_RUNTIME_DIR="$tmp/runtime"
export SCREEN_CAPTURE_SCREENSHOT_DIR="$tmp/shots"
export SCREEN_CAPTURE_VIDEO_DIR="$tmp/videos"
export SCREEN_CAPTURE_TEST_CLIPBOARD="$tmp/clipboard"
export SCREEN_CAPTURE_TEST_SIGNAL="$tmp/signal"
export SCREEN_CAPTURE_TEST_RECORDER_READY="$tmp/recorder-ready"
export SCREEN_CAPTURE_TEST_REAL_TIMEOUT="$timeout_bin"
export PATH="$fake:/usr/bin:/bin"

fail() {
  printf 'screen-capture: %s\n' "$*" >&2
  exit 1
}

cleanup_test() {
  local status=$?
  trap - EXIT INT TERM
  if [[ -n "$runner" ]]; then
    kill -TERM "$runner" 2>/dev/null || true
    wait "$runner" 2>/dev/null || true
  fi
  rm -rf -- "$tmp"
  return "$status"
}
trap cleanup_test EXIT

wait_for_file() {
  local path="$1" description="$2"
  for _ in {1..250}; do
    [[ -e "$path" ]] && return 0
    sleep 0.02
  done
  fail "timed out waiting for $description"
}

job_is_running() {
  local target="$1" job
  while read -r job; do
    [[ "$job" == "$target" ]] && return 0
  done < <(jobs -pr)
  return 1
}

cat > "$fake/maim" <<'SH'
#!/usr/bin/env bash
set -e
out="${@: -1}"
if [[ "${SCREEN_CAPTURE_TEST_MAIM_FAIL:-0}" == "1" ]]; then
  printf 'partial' > "$out"
  exit 1
fi
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
  printf '%s\n' "$1" > "${SCREEN_CAPTURE_TEST_SIGNAL:?}"
  printf 'video' > "$out"
  exit 0
}
trap 'finish INT' INT
trap 'finish TERM' TERM
printf 'ready\n' > "${SCREEN_CAPTURE_TEST_RECORDER_READY:?}"
while :; do sleep 0.05; done
SH

cat > "$fake/notify-send" <<'SH'
#!/usr/bin/env bash
exit 0
SH

cat > "$fake/timeout" <<'SH'
#!/usr/bin/env bash
if [[ -n "${SCREEN_CAPTURE_TEST_TIMEOUT_START_GATE:-}" ]]; then
  while [[ ! -e "$SCREEN_CAPTURE_TEST_TIMEOUT_START_GATE" ]]; do :; done
fi
exec "$SCREEN_CAPTURE_TEST_REAL_TIMEOUT" "$@"
SH

chmod +x "$fake"/*

region="$($subject screenshot-region)"
[[ -s "$region" ]]
[[ "$(cat "$SCREEN_CAPTURE_TEST_CLIPBOARD")" == png ]]

full="$($subject screenshot-screen)"
[[ -s "$full" ]]
[[ "$(cat "$SCREEN_CAPTURE_TEST_CLIPBOARD")" == png ]]

second_full="$($subject screenshot-screen)"
[[ "$second_full" != "$full" ]] || fail "two captures reused the same path"

before_failed_capture="$(find "$SCREEN_CAPTURE_SCREENSHOT_DIR" -type f | wc -l)"
if SCREEN_CAPTURE_TEST_MAIM_FAIL=1 "$subject" screenshot-region >/dev/null 2>&1; then
  fail "failed screenshot backend unexpectedly succeeded"
fi
after_failed_capture="$(find "$SCREEN_CAPTURE_SCREENSHOT_DIR" -type f | wc -l)"
[[ "$before_failed_capture" == "$after_failed_capture" ]] || fail "failed screenshot left a partial file"

empty="$tmp/custom/empty.mp4"
if SCREEN_CAPTURE_TEST_EMPTY=1 "$subject" record-window "$empty"; then
  fail "empty recorder output unexpectedly succeeded"
fi
[[ ! -e "$empty" ]]

if "$timeout_bin" --signal=TERM --kill-after=1s 4s \
  env SCREEN_CAPTURE_TEST_TIMEOUT_START_GATE="$tmp/never-start-timeout" \
  "$subject" record-window "$tmp/custom/not-started.mp4" >"$tmp/startup-failure.out" 2>&1; then
  fail "recorder supervisor unexpectedly started"
fi
grep -Fq "recorder supervisor failed to start" "$tmp/startup-failure.out" ||
  fail "recorder startup failure was not reported"
[[ ! -e "$SCREEN_CAPTURE_RUNTIME_DIR/recorder.pid" ]] || fail "failed startup left active PID state"

video="$tmp/custom/final.mp4"
$timeout_bin --signal=TERM --kill-after=1s 8s \
  env SCREEN_CAPTURE_TEST_TIMEOUT_START_GATE="$tmp/start-recorder" \
  "$subject" record-window "$video" > "$tmp/record.out" &
runner=$!
sleep 0.05
[[ ! -e "$SCREEN_CAPTURE_RUNTIME_DIR/recorder.pid" ]] || fail "published recorder PID before timeout started its child"
"$subject" stop
: > "$tmp/start-recorder"
wait_for_file "$SCREEN_CAPTURE_RUNTIME_DIR/recorder.pid" "active recorder PID"
wait_for_file "$SCREEN_CAPTURE_TEST_RECORDER_READY" "recorder signal handler"

[[ "$($subject status)" == recording$'\t'*"$video" ]]
if "$subject" record-window "$tmp/custom/second.mp4" >/dev/null 2>&1; then
  fail "second recorder started while one was active"
fi
"$subject" stop
for _ in {1..250}; do
  job_is_running "$runner" || break
  sleep 0.02
done
job_is_running "$runner" && fail "recorder did not exit after stop within 5 seconds"
if wait "$runner"; then
  runner=
else
  rc=$?
  runner=
  fail "recording command exited with status $rc"
fi
[[ -s "$video" ]]
[[ "$(cat "$SCREEN_CAPTURE_TEST_SIGNAL")" == INT ]] || fail "stop did not deliver SIGINT"
[[ "$(cat "$SCREEN_CAPTURE_TEST_CLIPBOARD")" == "$video" ]]
[[ "$($subject status 2>/dev/null || true)" == idle ]]

[[ -r "$qtile_capture_org" ]] || fail "missing canonical Qtile capture source"
[[ -r "$qtile_capture_py" ]] || fail "missing generated Qtile capture module"
awk '
  /^#\+begin_src python$/ { in_block = 1; next }
  /^#\+end_src$/ { in_block = 0; next }
  in_block { print }
' "$qtile_capture_org" > "$tmp/qtile_capture.py"
cmp -s "$tmp/qtile_capture.py" "$qtile_capture_py" || fail "Qtile capture literate/generated drift"

for command in screenshot-screen screenshot-region record-ui stop; do
  grep -Fq "screen-capture $command" "$qtile_capture_org" || fail "Org source missing $command binding"
  grep -Fq "screen-capture $command" "$qtile_capture_py" || fail "generated Qtile module missing $command binding"
done

if grep -qi 'spectacle' "$qtile_capture_org" "$qtile_capture_py" "$qtile_docs"; then
  fail "Spectacle remains in the active capture path or docs"
fi

printf 'screen-capture: ok\n'
