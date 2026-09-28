#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  zara-auth [--security-dir DIR] COMMAND [ARGS...]

Manage Zara's owner-private CURVE authentication state.

Commands:
  init                         Initialize auth state and print the server public key.
  public-key                   Print the server public key.
  list [--json]                List enrolled clients (pretty by default).
  enroll DEVICE_ID Z85_KEY     Enroll a client public key.
  enroll-file DEVICE_ID FILE   Enroll a public key read from FILE.
  enroll-stdin DEVICE_ID       Enroll a public key read from stdin.
  revoke DEVICE_ID             Revoke an enrolled device.
  status                       Show auth-state path, initialization, and client counts.
  path                         Print the active security-state directory.
  help                         Show this help.

Overrides:
  --security-dir DIR           Use DIR for this invocation.
  ZARA_SECURITY_DIR            Runtime override for the security-state directory.

The server private key and client registry remain in the security-state directory;
this command never copies credentials into the Nix store.
EOF
}

die() {
  printf 'zara-auth: %s\n' "$*" >&2
  exit 2
}

require_count() {
  local expected="$1"
  local command_name="$2"
  shift 2
  if (( $# != expected )); then
    die "$command_name expects $expected argument(s); run 'zara-auth help'"
  fi
}

default_security_dir="${ZARA_AUTH_DEFAULT_SECURITY_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}/zarathushtra/security}"
security_dir="${ZARA_SECURITY_DIR:-$default_security_dir}"

while (( $# > 0 )); do
  case "$1" in
    --security-dir)
      (( $# >= 2 )) || die "--security-dir requires a directory"
      security_dir="$2"
      shift 2
      ;;
    --security-dir=*)
      security_dir="${1#*=}"
      [[ -n "$security_dir" ]] || die "--security-dir requires a directory"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

command_name="${1:-help}"
if (( $# > 0 )); then
  shift
fi

run_security() {
  zara-server --security-dir "$security_dir" "$@"
}

case "$command_name" in
  init)
    require_count 0 init "$@"
    install -d -m 0700 "$security_dir"
    run_security --security-init
    ;;

  public-key|pubkey|key)
    require_count 0 public-key "$@"
    run_security --security-show-public-key
    ;;

  list|devices)
    case "${1:-}" in
      "")
        run_security --security-list-clients | jq .
        ;;
      --json)
        require_count 1 list "$@"
        run_security --security-list-clients
        ;;
      *)
        die "list accepts only --json"
        ;;
    esac
    ;;

  enroll|add)
    require_count 2 enroll "$@"
    device_id="$1"
    public_key="$2"
    [[ -n "$device_id" ]] || die "DEVICE_ID must not be empty"
    [[ -n "$public_key" ]] || die "Z85_KEY must not be empty"
    run_security --security-device-id "$device_id" "--security-enroll-key=$public_key"
    ;;

  enroll-file)
    require_count 2 enroll-file "$@"
    device_id="$1"
    key_file="$2"
    [[ -r "$key_file" ]] || die "cannot read key file: $key_file"
    public_key="$(tr -d '\r\n' < "$key_file")"
    [[ -n "$public_key" ]] || die "key file is empty: $key_file"
    run_security --security-device-id "$device_id" "--security-enroll-key=$public_key"
    ;;

  enroll-stdin)
    require_count 1 enroll-stdin "$@"
    device_id="$1"
    public_key=""
    IFS= read -r public_key || true
    [[ -n "$public_key" ]] || die "stdin did not contain a public key"
    run_security --security-device-id "$device_id" "--security-enroll-key=$public_key"
    ;;

  revoke|remove)
    require_count 1 revoke "$@"
    device_id="$1"
    [[ -n "$device_id" ]] || die "DEVICE_ID must not be empty"
    run_security --security-revoke-device "$device_id"
    ;;

  status)
    require_count 0 status "$@"
    printf 'security_dir=%s\n' "$security_dir"
    if public_key="$(run_security --security-show-public-key 2>/dev/null)"; then
      printf 'initialized=true\n'
      printf 'server_public_key=%s\n' "$public_key"
      if clients="$(run_security --security-list-clients 2>/dev/null)"; then
        printf 'clients_total=%s\n' "$(jq 'length' <<<"$clients")"
        printf 'clients_active=%s\n' "$(jq '[.[] | select(.active == true)] | length' <<<"$clients")"
      else
        printf 'clients_total=unavailable\n'
        printf 'clients_active=unavailable\n'
      fi
    else
      printf 'initialized=false\n'
    fi
    ;;

  path)
    require_count 0 path "$@"
    printf '%s\n' "$security_dir"
    ;;

  help)
    require_count 0 help "$@"
    usage
    ;;

  *)
    die "unknown command: $command_name; run 'zara-auth help'"
    ;;
esac
