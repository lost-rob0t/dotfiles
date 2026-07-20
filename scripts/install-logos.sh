#!/usr/bin/env bash
set -Eeuo pipefail

PROG="${0##*/}"

usage() {
  cat <<EOF
Usage: sudo $PROG [--system ATTR] [--flake PATH] [--disk NAME]
                  [--dry-run] [--write-efi-boot-entries] [--extra-args ARGS]
                  /dev/DEVICE
       sudo $PROG --list-systems
       sudo $PROG --help

Provision and install any NixOS system declared in the logos flake via
disko-install. disko owns partitioning, LUKS, Btrfs subvolumes, and the
resulting fileSystems/luks.devices entries; nixos-install then writes the
closure.

Arguments:
  /dev/DEVICE            Whole target disk (e.g. /dev/nvme0n1). Required.

Options:
  -h, --help             Show this help and exit.
  -l, --list-systems     Print every nixosConfigurations attr in the flake
                          and exit. Use this to discover installable systems.
  -s, --system ATTR      nixosConfigurations attr to install.
                          Default: logos. Examples: logos, desktop, laptop.
  -f, --flake PATH       Flake path (without #attr). The final URL passed to
                          disko-install is PATH#ATTR.
                          Default: /etc/logos on the ISO, or \$LOGOS_FLAKE.
  --disk NAME            disko disk name to overwrite (default: main).
                          Override only if the target system's disko config
                          uses a different top-level disk key.
  -n, --dry-run          Pass --dry-run to disko-install (no writes).
  --write-efi-boot-entries
                         Persist boot entry to host NVRAM. Off by default so
                          the installed disk stays portable across machines.
  --extra-args ARGS      Pass additional arguments verbatim to disko-install.

Environment:
  LOGOS_FLAKE            Override the default --flake path (no #attr).
  DISKO_INSTALL          Path to disko-install (default: resolved from PATH).

The target disk is destroyed. The LUKS passphrase (if the selected system
uses LUKS) is requested interactively at install time. Set a root password
afterwards with:
  nixos-enter --root /mnt -c 'passwd root'
EOF
}

die() {
  printf '%s: %s\n' "$PROG" "$*" >&2
  exit 1
}

device=""
dry_run=false
write_efi=false
list_systems=false
system_attr="${LOGOS_SYSTEM:-logos}"
flake_path="${LOGOS_FLAKE:-/etc/logos}"
disk_name="main"
extra_args=()

while (($#)); do
  case "$1" in
    -h | --help)
      usage
      exit 0
      ;;
    -l | --list-systems)
      list_systems=true
      ;;
    -s | --system)
      shift
      [[ $# -gt 0 ]] || die "--system requires a value"
      system_attr="$1"
      ;;
    --system=*)
      system_attr="${1#--system=}"
      ;;
    -f | --flake)
      shift
      [[ $# -gt 0 ]] || die "--flake requires a value"
      flake_path="$1"
      ;;
    --flake=*)
      flake_path="${1#--flake=}"
      ;;
    --disk)
      shift
      [[ $# -gt 0 ]] || die "--disk requires a value"
      disk_name="$1"
      ;;
    --disk=*)
      disk_name="${1#--disk=}"
      ;;
    -n | --dry-run)
      dry_run=true
      ;;
    --write-efi-boot-entries)
      write_efi=true
      ;;
    --extra-args)
      shift
      [[ $# -gt 0 ]] || die "--extra-args requires a value"
      extra_args+=("$1")
      ;;
    --)
      shift
      [[ $# -eq 1 ]] || die "expected exactly one device argument after --"
      device="$1"
      ;;
    -*)
      die "unknown option: $1 (try --help)"
      ;;
    *)
      [[ -z "$device" ]] || die "unexpected extra argument: $1"
      device="$1"
      ;;
  esac
  shift
done

flake_url="${flake_path}#${system_attr}"

# --list-systems: discover installable attrs from the flake, then exit.
if $list_systems; then
  if ! command -v nix >/dev/null 2>&1; then
    die "nix not found in PATH; needed for --list-systems"
  fi
  printf 'Flake: %s\n\n' "$flake_path"
  printf 'nixosConfigurations:\n'
  systems_json="$(nix --extra-experimental-features 'nix-command flakes' flake show --json "$flake_path" 2>/dev/null)" ||
    die "could not evaluate flake at $flake_path — is the path valid?"
  attrs="$(printf '%s\n' "$systems_json" | jq -r '.nixosConfigurations | keys[]' 2>/dev/null)" ||
    die "no nixosConfigurations attr found in $flake_path"
  while IFS= read -r attr; do
    printf '  %s\n' "$attr"
  done <<< "$attrs"
  exit 0
fi

[[ -n "$device" ]] || {
  usage >&2
  exit 2
}
[[ $EUID -eq 0 ]] || die "must run as root"
[[ -d /sys/firmware/efi ]] || die "must be booted in UEFI mode"

disko_install="${DISKO_INSTALL:-disko-install}"
command -v "$disko_install" >/dev/null 2>&1 ||
  die "disko-install not found in PATH; boot the logos ISO or run: nix run 'github:nix-community/disko/latest#disko-install'"

# Resolve the device to a real path and confirm it is a whole disk.
device="$(readlink -f "$device")"
[[ -b "$device" ]] || die "not a block device: $device"
[[ "$(lsblk -dnro TYPE "$device")" == "disk" ]] ||
  die "pass the whole disk, not a partition: $device"

# Refuse to operate on a disk that already has mounted filesystems.
if lsblk -nrpo MOUNTPOINTS "$device" | grep -q '[^[:space:]]'; then
  die "refusing to wipe a disk with mounted filesystems: $device"
fi

if $write_efi; then
  efi_status="written to NVRAM"
else
  efi_status="not written (portable)"
fi
if $dry_run; then
  dry_status="yes"
else
  dry_status="no"
fi

cat <<EOF

THIS DESTROYS ALL DATA ON $device

Flake:    $flake_url
System:  $system_attr
Disk:     $disk_name -> $device
EFI vars: $efi_status
Dry run:  $dry_status

The disk layout is declared by the selected system's disko config.
Use --dry-run to preview the exact partitioning/mount plan before committing.
EOF

read -r -p "Type WIPE-LOGOS to destroy the disk: " confirmation
[[ "$confirmation" == "WIPE-LOGOS" ]] || die "aborted"

args=(
  --flake "$flake_url"
  --disk "$disk_name"
  "$device"
)
$write_efi && args+=(--write-efi-boot-entries)
$dry_run && args+=(--dry-run)
[[ ${#extra_args[@]} -gt 0 ]] && args+=("${extra_args[@]}")

exec "$disko_install" "${args[@]}"
