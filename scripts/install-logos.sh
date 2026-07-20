#!/usr/bin/env bash
set -Eeuo pipefail

PROG="${0##*/}"

usage() {
  cat <<USAGE
Usage:
  $PROG [OPTIONS] --disk /dev/DEVICE
  $PROG [OPTIONS] /dev/DEVICE
  $PROG --list-systems
  $PROG --help

Install any Logos NixOS configuration with declarative LUKS + Btrfs disk
setup via disko-install. disko owns partitioning, LUKS2, Btrfs subvolumes,
swap, and the resulting fileSystems/luks.devices entries; nixos-install
then writes the closure.

Options:
  -d, --disk DEVICE              Whole target disk to erase and install to.
  -s, --system ATTR              nixosConfigurations attr to install.
                                  Default: logos. Examples: logos, desktop.
  -l, --list-systems             Print every installable system attr in the
                                  flake and exit. No root required.
  -f, --flake PATH               Flake path (without #attr). Final URL is
                                  PATH#ATTR. Default: \$LOGOS_DEFAULT_FLAKE
                                  or \$LOGOS_FLAKE or /etc/logos.
      --copy-source PATH         Copy a flake/source tree to /etc/nixos on
                                  the target. Default: \$LOGOS_SOURCE.
      --no-copy-source           Do not copy installer source to /etc/nixos.
      --mount-point PATH         Temporary installation mount point.
                                  Default: /mnt
      --mode MODE                disko-install mode: format or mount.
                                  Default: format
      --password-file PATH       Read the unseen user password from a file.
      --luks-password-file PATH  Read the LUKS passphrase from a file.
      --no-user-password         Do not set a password during installation.
      --no-write-efi-entries     Do not write EFI NVRAM boot entries.
      --swap [SIZE]              Create a Btrfs swapfile subvolume. SIZE
                                  defaults to 8G. For hibernation, set SIZE
                                  >= total RAM.
      --swap-hibernate [SIZE]    Like --swap but also enable hibernation
                                  (sets resume= and resume_offset=). SIZE
                                  defaults to 8G and must be >= RAM.
      --hardware-config          Copy generated hardware-configuration.nix
                                  into the user dotrepo at
                                  /etc/nixos/nix/nixos/hosts/<system>/ before
                                  install. Default: on (use
                                  --no-hardware-config to skip).
      --home-manager             After nixos-install, su to the unseen user
                                  and run the home-manager activation for
                                  their system. Default: on (use
                                  --no-home-manager to skip).
      --dry-run                  Build and print the install actions only.
      --show-trace               Include Nix evaluation traces on failure.
  -y, --yes                      Skip the Logos destructive confirmation.
  -h, --help                     Show this help and exit.

Environment:
  LOGOS_DEFAULT_FLAKE            Flake URI#ATTR (set by the nix package).
  LOGOS_SOURCE                   Source tree to copy to /etc/nixos.
  LOGOS_FLAKE                    Override the default --flake path.
  LOGOS_SYSTEM                   Override the default --system attr.
  LOGOS_LUKS_PASSWORD_FILE       Path to LUKS passphrase file.
  DISKO_INSTALL                  Path to disko-install binary.

Examples:
  sudo install-logos --disk /dev/nvme0n1
  sudo install-logos --system desktop --disk /dev/sda
  sudo install-logos --swap-hibernate 16G --disk /dev/nvme0n1
  sudo nix run .#install-logos -- --disk /dev/nvme0n1
  install-logos --list-systems
  install-logos --dry-run --disk /dev/nvme0n1

The format mode destroys the selected disk and creates:
  - 1 GiB EFI system partition
  - LUKS2 encrypted root partition
  - Btrfs subvolumes: @, @home, @nix, @log, @snapshots
  - (optional) @swap subvolume with a swapfile when --swap is passed
USAGE
}

die() {
  printf '%s: %s\n' "$PROG" "$*" >&2
  exit 1
}

require_value() {
  local option="$1"
  local value="${2-}"
  [[ -n "$value" ]] || die "$option requires a value"
}

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/.." && pwd)"
DEFAULT_FLAKE="${LOGOS_DEFAULT_FLAKE:-${LOGOS_FLAKE:-/etc/logos}#logos}"
DEFAULT_SOURCE="${LOGOS_SOURCE:-$REPO_ROOT}"
DISKO_INSTALL_BIN="${DISKO_INSTALL:-disko-install}"

disk=""
system_attr="${LOGOS_SYSTEM:-logos}"
flake="$DEFAULT_FLAKE"
copy_source="$DEFAULT_SOURCE"
mount_point="/mnt"
mode="format"
dry_run=false
assume_yes=false
write_efi_entries=true
show_trace=false
password_file=""
luks_password_file="${LOGOS_LUKS_PASSWORD_FILE:-}"
set_user_password=true
list_systems=false
swap_enable=false
swap_hibernate=false
swap_size="8G"
copy_hardware_config=true
run_home_manager=true

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h | --help)
      usage
      exit 0
      ;;
    -l | --list-systems)
      list_systems=true
      shift
      ;;
    -d | --disk)
      require_value "$1" "${2-}"
      disk="$2"
      shift 2
      ;;
    --disk=*)
      disk="${1#--disk=}"
      shift
      ;;
    -s | --system)
      require_value "$1" "${2-}"
      system_attr="$2"
      shift 2
      ;;
    --system=*)
      system_attr="${1#--system=}"
      shift
      ;;
    -f | --flake)
      require_value "$1" "${2-}"
      flake="$2"
      shift 2
      ;;
    --flake=*)
      flake="${1#--flake=}"
      shift
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
    --swap)
      swap_enable=true
      # Optional next arg is a size token (must contain a digit).
      if [[ $# -ge 2 && "$2" =~ ^[0-9]+[KMG]?$ ]]; then
        swap_size="$2"
        shift 2
      else
        shift
      fi
      ;;
    --swap=*)
      swap_enable=true
      swap_size="${1#--swap=}"
      shift
      ;;
    --swap-hibernate)
      swap_hibernate=true
      swap_enable=true
      if [[ $# -ge 2 && "$2" =~ ^[0-9]+[KMG]?$ ]]; then
        swap_size="$2"
        shift 2
      else
        shift
      fi
      ;;
    --swap-hibernate=*)
      swap_hibernate=true
      swap_enable=true
      swap_size="${1#--swap-hibernate=}"
      shift
      ;;
    --hardware-config)
      copy_hardware_config=true
      shift
      ;;
    --no-hardware-config)
      copy_hardware_config=false
      shift
      ;;
    --home-manager)
      run_home_manager=true
      shift
      ;;
    --no-home-manager)
      run_home_manager=false
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

# --list-systems: discover installable attrs from the flake, then exit.
# No root required.
if $list_systems; then
  if ! command -v nix >/dev/null 2>&1; then
    die "nix not found in PATH; needed for --list-systems"
  fi
  flake_path="${flake%%#*}"
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

[[ -n "$disk" ]] || {
  usage >&2
  exit 2
}

case "$mode" in
  format | mount) ;;
  *) die "invalid mode '$mode'; expected format or mount" ;;
esac

# Split the flake URI into path and attr, then apply --system if set.
flake_path="${flake%%#*}"
flake_attr="${flake##*#}"
# If the user passed --system, override the attr portion.
if [[ -n "$system_attr" && "$system_attr" != "${flake_attr:-logos}" ]]; then
  flake_attr="$system_attr"
  flake="${flake_path}#${flake_attr}"
fi
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
  [[ "$(lsblk -dnro TYPE "$disk")" == "disk" ]] ||
    die "pass the whole disk, not a partition: $disk"

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
  LOGOS_LUKS_PASSWORD_FILE="$(readlink -f -- "$luks_password_file")"
  export LOGOS_LUKS_PASSWORD_FILE
fi

# --- Build the ad-hoc system-config overlay ---------------------------------
# disko-install accepts --system-config as a NixOS config JSON snippet that
# is merged on top of the flake's attribute. We use it to:
#   - set the unseen user's hashed password
#   - enable swap / hibernation via the logos.disk.swap option
# This keeps the disk declaration flake-native while letting the installer
# customize per-run concerns without editing the repo.

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
  system_config="$(jq -cn --arg hash "$password_hash" \
    '{users:{users:{unseen:{hashedPassword:$hash}}}}')"
  unset password_hash
fi

if $swap_enable; then
  if $swap_hibernate; then
    hibernate_flag=true
  else
    hibernate_flag=false
  fi
  swap_json="$(jq -cn \
    --argjson enable true \
    --argjson hibernate "$hibernate_flag" \
    --arg size "$swap_size" \
    '{logos:{disk:{swap:{enable:$enable, hibernate:$hibernate, size:$size}}}}')"
  if [[ -n "$system_config" ]]; then
    system_config="$(jq -cn --argjson a "$system_config" --argjson b "$swap_json" '$a * $b')"
  else
    system_config="$swap_json"
  fi
fi

# --- hardware-configuration copy into the user dotrepo ----------------------
# Generate hardware-configuration.nix from the running ISO and inject it
# into the source tree before disko-install copies it to /etc/nixos, so the
# installed system has a tracked hardware config matching the target machine.

if $copy_hardware_config && [[ "$dry_run" == false && -n "$copy_source" ]]; then
  hw_dest="$copy_source/nix/nixos/hosts/${flake_attr}/hardware-configuration.nix"
  printf ':: Generating hardware-configuration.nix -> %s\n' "$hw_dest"
  mkdir -p "$(dirname -- "$hw_dest")"
  nixos-generate-config --root / --show-hardware-config > "$hw_dest"
fi

# --- Run disko-install -------------------------------------------------------

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

if [[ "$dry_run" == true ]]; then
  printf '\nLogos install plan printed (dry-run).\n'
  exit 0
fi

# --- Compute and apply the resume_offset for hibernation --------------------
# disko created the swapfile at /.swapvol/swapfile; we need its physical
# offset (in 4096-byte pages) so the kernel can resume from it.

if $swap_hibernate; then
  swapfile="$mount_point/.swapvol/swapfile"
  if [[ -f "$swapfile" ]]; then
    resume_offset="$(filefrag -e "$swapfile" 2>/dev/null \
      | awk 'NR==1{next} /extended:/ {gsub(/:/,"",$3); print $3; exit}')"
    if [[ -n "$resume_offset" && "$resume_offset" =~ ^[0-9]+$ ]]; then
      printf ':: hibernation resume_offset: %s\n' "$resume_offset"
      # Persist the offset into the installed system's config by appending
      # to the cloned dotrepo before it's activated on next boot.
      if [[ -n "$copy_source" ]]; then
        hib_overlay="$copy_source/nix/nixos/hosts/${flake_attr}/hibernation-offset.nix"
        cat > "$hib_overlay" <<EOF
# Auto-generated by install-logos on $(date -u +%FT%TZ).
# Physical offset of /.swapvol/swapfile, in 4096-byte pages.
{ ... }: {
  logos.hibernation.resumeOffset = $resume_offset;
}
EOF
        printf ':: wrote %s\n' "$hib_overlay"
      fi
    else
      printf ':: warning: could not compute resume_offset for %s; hibernation may not resume\n' \
        "$swapfile" >&2
    fi
  else
    printf ':: warning: swapfile not found at %s; skipping resume_offset\n' "$swapfile" >&2
  fi
fi

# --- su to the unseen user and run home-manager activation ------------------
# The installed system has the dotrepo at /etc/nixos. We chroot into the
# target via nixos-enter and run the home-manager switch as unseen.

if $run_home_manager; then
  printf '\n:: Activating home-manager for unseen on the target...\n'
  if ! nixos-enter --root "$mount_point" -- \
    su - unseen -c 'nix run --extra-experimental-features "nix-command flakes" \
      home-manager/release-24.11 -- switch \
      --flake /etc/nixos#unseen'; then
    printf ':: warning: home-manager activation failed; run manually after reboot:\n' >&2
    printf '   su - unseen -c "nix run home-manager/release-24.11 -- switch --flake /etc/nixos#unseen"\n' >&2
  fi
fi

printf '\nLogos installation completed.\n'
if [[ "$set_user_password" == true ]]; then
  printf 'The unseen user password was set during installation.\n'
fi
if $swap_enable; then
  printf 'Swap enabled (%s).\n' "$swap_size"
  $swap_hibernate && printf 'Hibernation support enabled.\n'
fi
printf 'The installed flake is at: %s\n' "$mount_point/etc/nixos"
