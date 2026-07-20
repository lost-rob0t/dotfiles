#!/usr/bin/env bash
set -Eeuo pipefail

usage() {
  cat <<'USAGE'
Usage:
  install-logos [OPTIONS] --disk /dev/DEVICE
  install-logos [OPTIONS] /dev/DEVICE

Install the Logos NixOS configuration with declarative LUKS + Btrfs disk setup.

Options:
  -d, --disk DEVICE              Whole target disk to erase and install to.
  -f, --flake URI#ATTR           Flake configuration to install.
                                  Default: the bundled flake, #logos.
      --copy-source PATH         Copy a flake/source tree to /etc/nixos.
      --no-copy-source           Do not copy installer source to /etc/nixos.
      --mount-point PATH         Temporary installation mount point.
                                  Default: /mnt
      --mode MODE                disko-install mode: format or mount.
                                  Default: format
      --password-file PATH       Read the unseen user password from a file.
      --luks-password-file PATH  Read the LUKS passphrase from a file.
      --no-user-password         Do not set a password during installation.
      --no-write-efi-entries     Do not write EFI NVRAM boot entries.
      --dry-run                  Build and print the install actions only.
      --show-trace               Include Nix evaluation traces on failure.
  -y, --yes                      Skip the Logos destructive confirmation.
  -h, --help                     Show this help and exit.

Examples:
  sudo install-logos --disk /dev/nvme0n1
  sudo install-logos --flake /etc/logos#logos --disk /dev/sda
  sudo nix run .#install-logos -- --disk /dev/nvme0n1
  install-logos --dry-run --disk /dev/nvme0n1

The format mode destroys the selected disk and creates:
  - 1 GiB EFI system partition
  - LUKS2 encrypted root partition
  - Btrfs subvolumes: @, @home, @nix, @log, @snapshots
USAGE
}

die() {
  printf 'install-logos: %s\n' "$*" >&2
  exit 1
}

require_value() {
  local option="$1"
  local value="${2-}"
  [[ -n "$value" ]] || die "$option requires a value"
}

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
DEFAULT_FLAKE="${LOGOS_DEFAULT_FLAKE:-${REPO_ROOT}#logos}"
DEFAULT_SOURCE="${LOGOS_SOURCE:-$REPO_ROOT}"
DISKO_INSTALL_BIN="${DISKO_INSTALL:-disko-install}"

disk=""
flake="$DEFAULT_FLAKE"
copy_source="$DEFAULT_SOURCE"
mount_point="/mnt"
mode="format"
dry_run=false
assume_yes=false
write_efi_entries=true
show_trace=false
password_file=""
luks_password_file=""
set_user_password=true

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h | --help)
      usage
      exit 0
      ;;
    -d | --disk)
      require_value "$1" "${2-}"
      disk="$2"
      shift 2
      ;;
    -f | --flake)
      require_value "$1" "${2-}"
      flake="$2"
      shift 2
      ;;
    --copy-source)
      require_value "$1" "${2-}"
      copy_source="$2"
      shift 2
      ;;
    --no-copy-source)
      copy_source=""
      shift
      ;;
    --mount-point)
      require_value "$1" "${2-}"
      mount_point="$2"
      shift 2
      ;;
    --mode)
      require_value "$1" "${2-}"
      mode="$2"
      shift 2
      ;;
    --password-file)
      require_value "$1" "${2-}"
      password_file="$2"
      shift 2
      ;;
    --luks-password-file)
      require_value "$1" "${2-}"
      luks_password_file="$2"
      shift 2
      ;;
    --no-user-password)
      set_user_password=false
      shift
      ;;
    --no-write-efi-entries)
      write_efi_entries=false
      shift
      ;;
    --dry-run)
      dry_run=true
      shift
      ;;
    --show-trace)
      show_trace=true
      shift
      ;;
    -y | --yes)
      assume_yes=true
      shift
      ;;
    --)
      shift
      break
      ;;
    -*)
      die "unknown option: $1 (run --help)"
      ;;
    *)
      [[ -z "$disk" ]] || die "multiple target disks supplied"
      disk="$1"
      shift
      ;;
  esac
done

if [[ $# -gt 0 ]]; then
  [[ -z "$disk" && $# -eq 1 ]] || die "unexpected arguments: $*"
  disk="$1"
fi

[[ -n "$disk" ]] || {
  usage >&2
  exit 2
}

case "$mode" in
  format | mount) ;;
  *) die "invalid mode '$mode'; expected format or mount" ;;
esac

[[ "$flake" == *#* ]] || die "flake must include a configuration fragment, such as path#logos"

if [[ -n "$copy_source" ]]; then
  copy_source="$(readlink -f -- "$copy_source")"
  [[ -d "$copy_source" ]] || die "copy source is not a directory: $copy_source"
fi

if [[ -e "$disk" ]]; then
  disk="$(readlink -f -- "$disk")"
fi

if [[ "$dry_run" == false ]]; then
  [[ $EUID -eq 0 ]] || die "run as root (or use --dry-run)"
  [[ -d /sys/firmware/efi ]] || die "installer must be booted in UEFI mode"
  [[ -b "$disk" ]] || die "not a block device: $disk"
  [[ "$(lsblk -dnro TYPE "$disk")" == "disk" ]] || die "pass the whole disk, not a partition: $disk"

  if lsblk -nrpo MOUNTPOINTS "$disk" | grep -q '[^[:space:]]'; then
    lsblk "$disk" >&2
    die "refusing to erase a disk with mounted filesystems: $disk"
  fi
fi

if [[ "$dry_run" == false && "$assume_yes" == false && "$mode" == "format" ]]; then
  cat <<EOF
THIS DESTROYS ALL DATA ON $disk

The flake $flake will create an encrypted LUKS2 + Btrfs Logos installation.
EOF
  lsblk "$disk"
  printf '\nType the exact disk path to continue: '
  read -r confirmation
  [[ "$confirmation" == "$disk" ]] || die "confirmation mismatch"
  printf 'Type WIPE-LOGOS to destroy the disk: '
  read -r confirmation
  [[ "$confirmation" == "WIPE-LOGOS" ]] || die "aborted"
fi

if [[ -n "$luks_password_file" ]]; then
  [[ -r "$luks_password_file" ]] || die "LUKS password file is not readable: $luks_password_file"
  export LOGOS_LUKS_PASSWORD_FILE="$(readlink -f -- "$luks_password_file")"
fi

system_config=""
if [[ "$dry_run" == false && "$set_user_password" == true ]]; then
  if [[ -n "$password_file" ]]; then
    [[ -r "$password_file" ]] || die "password file is not readable: $password_file"
    IFS= read -r password < "$password_file"
  else
    [[ -t 0 && -t 1 ]] || die "no terminal available; use --password-file or --no-user-password"
    while true; do
      read -r -s -p "Password for unseen: " password
      printf '\n'
      read -r -s -p "Confirm password: " password_confirmation
      printf '\n'
      [[ -n "$password" ]] || { printf 'Password cannot be empty.\n' >&2; continue; }
      [[ "$password" == "$password_confirmation" ]] || { printf 'Passwords do not match.\n' >&2; continue; }
      break
    done
    unset password_confirmation
  fi

  [[ -n "$password" ]] || die "password cannot be empty"
  password_hash="$(printf '%s\n' "$password" | openssl passwd -6 -stdin)"
  unset password
  system_config="$(jq -cn --arg hash "$password_hash" '{users:{users:{unseen:{hashedPassword:$hash}}}}')"
  unset password_hash
fi

command=(
  "$DISKO_INSTALL_BIN"
  --mode "$mode"
  --flake "$flake"
  --disk main "$disk"
  --mount-point "$mount_point"
)

[[ "$write_efi_entries" == true ]] && command+=(--write-efi-boot-entries)
[[ "$dry_run" == true ]] && command+=(--dry-run)
[[ "$show_trace" == true ]] && command+=(--show-trace)
[[ -n "$copy_source" ]] && command+=(--extra-files "$copy_source" /etc/nixos)
[[ -n "$system_config" ]] && command+=(--system-config "$system_config")

printf 'Running:'
printf ' %q' "${command[@]}"
printf '\n'

"${command[@]}"

if [[ "$dry_run" == false ]]; then
  printf '\nLogos installation completed.\n'
  if [[ "$set_user_password" == true ]]; then
    printf 'The unseen user password was set during installation.\n'
  fi
fi
