#!/usr/bin/env bash
set -Eeuo pipefail

usage() {
  echo "usage: sudo $0 /dev/DEVICE" >&2
  exit 2
}

[[ $# -eq 1 ]] || usage
[[ $EUID -eq 0 ]] || { echo "run as root" >&2; exit 1; }
[[ -d /sys/firmware/efi ]] || {
  echo "the installer must be booted in UEFI mode" >&2
  exit 1
}

for command in btrfs cryptsetup lsblk mkfs.btrfs mkfs.fat mount mountpoint parted partprobe readlink udevadm umount wipefs; do
  command -v "$command" >/dev/null || {
    echo "missing required command: $command" >&2
    exit 1
  }
done

DISK="$(readlink -f "$1")"
MOUNT_POINT="${MOUNT_POINT:-/mnt}"
MAPPER_NAME="${MAPPER_NAME:-cryptroot}"
MAPPER_DEVICE="/dev/mapper/$MAPPER_NAME"

[[ -b "$DISK" ]] || { echo "not a block device: $DISK" >&2; exit 1; }
[[ "$(lsblk -dnro TYPE "$DISK")" == "disk" ]] || {
  echo "pass the whole disk, not a partition: $DISK" >&2
  exit 1
}

if lsblk -nrpo MOUNTPOINTS "$DISK" | grep -q '[^[:space:]]'; then
  echo "refusing to wipe a disk with mounted filesystems: $DISK" >&2
  lsblk "$DISK" >&2
  exit 1
fi

if cryptsetup status "$MAPPER_NAME" >/dev/null 2>&1; then
  echo "mapper already open: $MAPPER_NAME" >&2
  exit 1
fi

partition_path() {
  local disk="$1"
  local number="$2"

  if [[ "$disk" =~ [0-9]$ ]]; then
    printf '%sp%s\n' "$disk" "$number"
  else
    printf '%s%s\n' "$disk" "$number"
  fi
}

EFI_PART="$(partition_path "$DISK" 1)"
CRYPT_PART="$(partition_path "$DISK" 2)"

cat <<EOF
THIS DESTROYS ALL DATA ON $DISK

Layout:
  $EFI_PART    1 GiB EFI system partition
  $CRYPT_PART  remaining space, LUKS2 + Btrfs

Btrfs subvolumes share one encrypted filesystem:
  @  @home  @nix  @log  @snapshots
EOF

read -r -p "Type the exact disk path to continue: " confirmation
[[ "$confirmation" == "$DISK" ]] || { echo "confirmation mismatch" >&2; exit 1; }

read -r -p "Type WIPE-LOGOS to destroy the disk: " confirmation
[[ "$confirmation" == "WIPE-LOGOS" ]] || { echo "aborted" >&2; exit 1; }

wipefs --all --force "$DISK"
parted --script "$DISK" \
  mklabel gpt \
  mkpart EFI fat32 1MiB 1025MiB \
  set 1 esp on \
  name 1 EFI \
  mkpart cryptroot 1025MiB 100% \
  name 2 cryptroot

partprobe "$DISK"
udevadm settle

[[ -b "$EFI_PART" && -b "$CRYPT_PART" ]] || {
  echo "partition devices did not appear" >&2
  exit 1
}

mkfs.fat -F 32 -n EFI "$EFI_PART"
cryptsetup luksFormat --type luks2 --label logos "$CRYPT_PART"
cryptsetup open "$CRYPT_PART" "$MAPPER_NAME"
mkfs.btrfs -f -L logos "$MAPPER_DEVICE"

mkdir -p "$MOUNT_POINT"
mount "$MAPPER_DEVICE" "$MOUNT_POINT"

for subvolume in @ @home @nix @log @snapshots; do
  btrfs subvolume create "$MOUNT_POINT/$subvolume"
done

umount "$MOUNT_POINT"

mount_options="compress=zstd:3,discard=async,noatime,space_cache=v2"
mount -o "$mount_options,subvol=@" "$MAPPER_DEVICE" "$MOUNT_POINT"
mkdir -p "$MOUNT_POINT"/{boot,home,nix,var/log,.snapshots}
mount -o "$mount_options,subvol=@home" "$MAPPER_DEVICE" "$MOUNT_POINT/home"
mount -o "$mount_options,subvol=@nix" "$MAPPER_DEVICE" "$MOUNT_POINT/nix"
mount -o "$mount_options,subvol=@log" "$MAPPER_DEVICE" "$MOUNT_POINT/var/log"
mount -o "$mount_options,subvol=@snapshots" "$MAPPER_DEVICE" "$MOUNT_POINT/.snapshots"
mount "$EFI_PART" "$MOUNT_POINT/boot"

cat <<EOF

Provisioning complete.
Mounted target at $MOUNT_POINT.
Run install-logos $DISK to provision and install in one pass next time,
or continue manually with the flake under /etc/logos.
EOF
