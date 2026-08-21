#!/usr/bin/env bash
set -uo pipefail

umask 077

: "${HM_UPDATER_DATA_DIR:?HM_UPDATER_DATA_DIR is required}"
: "${HM_UPDATER_REPOSITORY:?HM_UPDATER_REPOSITORY is required}"
: "${HM_UPDATER_REMOTE_URL:?HM_UPDATER_REMOTE_URL is required}"
: "${HM_UPDATER_BRANCH:?HM_UPDATER_BRANCH is required}"
: "${HM_UPDATER_CONFIGURATION:?HM_UPDATER_CONFIGURATION is required}"
: "${HM_UPDATER_LOGROTATE_CONFIG:?HM_UPDATER_LOGROTATE_CONFIG is required}"

DATA_DIR="$HM_UPDATER_DATA_DIR"
CHECKOUT="$DATA_DIR/repo"
LOG_FILE="$DATA_DIR/update.log"
FAILURE_FILE="$DATA_DIR/failure.txt"
NOTIFICATION_ID_FILE="$DATA_DIR/notification-id"
ISSUE_TITLE="Home Manager auto-update failed: $HM_UPDATER_CONFIGURATION"

mkdir -p "$DATA_DIR"
logrotate -s "$DATA_DIR/logrotate.state" "$HM_UPDATER_LOGROTATE_CONFIG" || true
exec >>"$LOG_FILE" 2>&1

log() {
  printf '[%s] %s\n' "$(date --iso-8601=seconds)" "$*"
}

current_generation() {
  home-manager generations 2>/dev/null | sed -n '1s/.* -> //p'
}

find_issue_number() {
  local state="${1:-all}"
  local json
  if ! json="$(gh issue list --repo "$HM_UPDATER_REPOSITORY" --state "$state" --limit 100 --json number,title 2>/dev/null)"; then
    return 1
  fi
  printf '%s\n' "$json" | jq -r --arg title "$ISSUE_TITLE" '.[] | select(.title == $title) | .number' | head -n1
}

report_issue() {
  local stage="$1"
  local detail="$2"
  local issue_number body commit_sha

  if ! gh auth status --hostname github.com >/dev/null 2>&1; then
    log "WARNING: gh is not authenticated; cannot create/update failure issue"
    return 1
  fi

  commit_sha="$(git -C "$CHECKOUT" rev-parse HEAD 2>/dev/null || printf 'unknown')"
  body=$(cat <<EOF
The automatic Home Manager update for \`$HM_UPDATER_CONFIGURATION\` failed.

- Stage: \`$stage\`
- Branch: \`$HM_UPDATER_BRANCH\`
- Commit: \`$commit_sha\`
- Host configuration: \`$HM_UPDATER_CONFIGURATION\`
- Local log: \`$LOG_FILE\`
- Last failure: \`$(date --iso-8601=seconds)\`

$detail

This is the canonical updater failure issue. Subsequent failures update/reopen this issue instead of creating duplicates.
EOF
)

  issue_number="$(find_issue_number all || true)"
  if [[ -n "$issue_number" ]]; then
    gh issue reopen "$issue_number" --repo "$HM_UPDATER_REPOSITORY" >/dev/null 2>&1 || true
    gh issue edit "$issue_number" --repo "$HM_UPDATER_REPOSITORY" --body "$body" >/dev/null
    log "Updated GitHub issue #$issue_number"
  else
    gh issue create --repo "$HM_UPDATER_REPOSITORY" --title "$ISSUE_TITLE" --body "$body" >/dev/null
    log "Created GitHub issue: $ISSUE_TITLE"
  fi
}

close_issue() {
  local issue_number
  if ! gh auth status --hostname github.com >/dev/null 2>&1; then
    return 0
  fi
  issue_number="$(find_issue_number open || true)"
  if [[ -n "$issue_number" ]]; then
    gh issue close "$issue_number" --repo "$HM_UPDATER_REPOSITORY" \
      --comment "Automatic update recovered successfully at $(date --iso-8601=seconds)." >/dev/null || true
    log "Closed recovered GitHub issue #$issue_number"
  fi
}

clear_failure_notification() {
  local notification_id
  rm -f "$FAILURE_FILE"
  if [[ -s "$NOTIFICATION_ID_FILE" ]]; then
    notification_id="$(cat "$NOTIFICATION_ID_FILE")"
    notify-send --replace-id="$notification_id" --expire-time=1 \
      --app-name="Home Manager Updater" \
      "Home Manager updater recovered" \
      "$HM_UPDATER_CONFIGURATION updated successfully." >/dev/null 2>&1 || true
    rm -f "$NOTIFICATION_ID_FILE"
  fi
}

fail_update() {
  local stage="$1"
  local detail="$2"
  {
    printf 'Home Manager update failed for %s\n' "$HM_UPDATER_CONFIGURATION"
    printf 'Stage: %s\n' "$stage"
    printf 'Time: %s\n' "$(date --iso-8601=seconds)"
    printf '%s\n' "$detail"
    printf 'Log: %s\n' "$LOG_FILE"
  } >"$FAILURE_FILE"
  log "FAILURE [$stage]: $detail"
  report_issue "$stage" "$detail" || true
  exit 1
}

log "Starting Home Manager update for $HM_UPDATER_CONFIGURATION"

if [[ ! -d "$CHECKOUT/.git" ]]; then
  rm -rf "$CHECKOUT"
  if ! git clone --no-checkout "$HM_UPDATER_REMOTE_URL" "$CHECKOUT"; then
    fail_update "checkout" "Could not clone $HM_UPDATER_REMOTE_URL."
  fi
fi

if ! git -C "$CHECKOUT" remote set-url origin "$HM_UPDATER_REMOTE_URL"; then
  fail_update "checkout" "Could not configure the updater checkout remote."
fi
if ! git -C "$CHECKOUT" fetch --prune origin "$HM_UPDATER_BRANCH"; then
  fail_update "fetch" "Could not fetch origin/$HM_UPDATER_BRANCH."
fi
if ! git -C "$CHECKOUT" checkout -B "$HM_UPDATER_BRANCH" "origin/$HM_UPDATER_BRANCH"; then
  fail_update "checkout" "Could not reset the dedicated checkout to origin/$HM_UPDATER_BRANCH."
fi
git -C "$CHECKOUT" reset --hard "origin/$HM_UPDATER_BRANCH"
git -C "$CHECKOUT" clean -fdx

flake_attr="$CHECKOUT#homeConfigurations.\"$HM_UPDATER_CONFIGURATION\".activationPackage"
if ! nix build --no-link --show-trace "$flake_attr"; then
  fail_update "build" "The candidate Home Manager activation package failed to build. The active generation was not changed."
fi

before_generation="$(current_generation)"
log "Current generation before activation: ${before_generation:-unknown}"

if ! home-manager switch --flake "$CHECKOUT#$HM_UPDATER_CONFIGURATION" --show-trace; then
  after_generation="$(current_generation)"
  log "Generation after failed activation: ${after_generation:-unknown}"

  if [[ -n "$before_generation" && -n "$after_generation" && "$after_generation" != "$before_generation" ]]; then
    log "Activation advanced the generation; rolling back once"
    if ! home-manager switch --rollback; then
      fail_update "rollback" "Activation failed and the one-shot rollback also failed. Manual recovery is required."
    fi
    rolled_generation="$(current_generation)"
    if [[ "$rolled_generation" != "$before_generation" ]]; then
      fail_update "rollback" "Rollback command completed but did not restore the previous generation ($before_generation). Current: ${rolled_generation:-unknown}."
    fi
    fail_update "activation" "Activation failed. The previous Home Manager generation was restored successfully."
  fi

  fail_update "activation" "Activation failed before the Home Manager generation advanced, so no rollback was necessary."
fi

clear_failure_notification
close_issue
log "Home Manager update completed successfully"
