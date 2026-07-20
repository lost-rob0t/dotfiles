#!/usr/bin/env bash
# flash-logos: build the logos installer ISO and write it to a selected disk.
#
# Flow:
#   1. nix build .#logos-iso --no-link (no result symlink, GC-able)
#   2. print the ISO path in the store
#   3. fzf-select a target disk (all disks shown, no pre-filter)
#   4. refuse if the disk has mounted filesystems or hosts the active root
#   5. three escalating confirmation prompts
#   6. dd the ISO onto the selected disk
set -Eeuo pipefail

PROG="${0##*/}"

usage() {
  cat <<EOF
Usage: $PROG [--no-build] [--no-flash] [--dry-run] [--device PATH]
            [--flake PATH] [--extra-args ARGS]
       $PROG --help

Build the logos installer ISO and flash it to a disk.

Options:
  -h, --help            Show this help and exit.
  -n, --dry-run         Stop after selecting the disk; show what would run.
  --no-build            Skip the nix build step; assume the ISO is already
                        built. Use with --device for non-interactive reruns.
  --no-flash            Build and print the ISO path only; do not write.
  --device PATH         Preselect the target disk (skip fzf). Useful for
                        re-running the script on the same disk.
  --flake PATH         Flake path to build from (default: current dir).
  --extra-args ARGS    Extra arguments passed to nix build.

Environment:
  FZF                   Override the fzf binary (default: resolved from PATH).

Safety:
  - All disks are listed for selection (no name pre-filtering).
  - The script refuses to write to a disk that has any mounted filesystem
    (including the active root). Unmount first, or pick a different disk.
  - Three escalating confirmation prompts must all pass before dd runs.
  - dd uses oflag=direct,sync and bs=4M for a safe, verifiable write.
  - nix build runs as the calling user; dd is escalated via sudo
    automatically. Run 'nix run .#flash-logos' without sudo.
EOF
}

die() {
  printf '%s: %s\n' "$PROG" "$*" >&2
  exit 1
}

build=true
flash=true
dry_run=false
preselected_device=""
flake_path="."
extra_args=()

while (($#)); do
  case "$1" in
    -h | --help)
      usage
      exit 0
      ;;
    -n | --dry-run)
      dry_run=true
      ;;
    --no-build)
      build=false
      ;;
    --no-flash)
      flash=false
      ;;
    --device)
      shift
      [[ $# -gt 0 ]] || die "--device requires a value"
      preselected_device="$1"
      ;;
    --device=*)
      preselected_device="${1#--device=}"
      ;;
    --flake)
      shift
      [[ $# -gt 0 ]] || die "--flake requires a value"
      flake_path="$1"
      ;;
    --flake=*)
      flake_path="${1#--flake=}"
      ;;
    --extra-args)
      shift
      [[ $# -gt 0 ]] || die "--extra-args requires a value"
      extra_args+=("$1")
      ;;
    --)
      shift
      break
      ;;
    -*)
      die "unknown option: $1 (try --help)"
      ;;
    *)
      die "unexpected argument: $1 (try --help)"
      ;;
  esac
  shift
done

[[ -d /sys/firmware/efi ]] || die "must be booted in UEFI mode"

# --- 1. Build (unless skipped) ------------------------------------------------

iso_path=""
if $build; then
  printf ':: Building %s#logos-iso (no result symlink)\n' "$flake_path"
  build_args=(--no-link --print-build-logs)
  [[ ${#extra_args[@]} -gt 0 ]] && build_args+=("${extra_args[@]}")
  nix build "$flake_path#logos-iso" "${build_args[@]}"
fi

iso_path="$(nix path-info "$flake_path#logos-iso")"
iso_file="$(find "$iso_path/iso" -maxdepth 1 -type f -name '*.iso' -print -quit)"
[[ -n "$iso_file" && -f "$iso_file" ]] ||
  die "no .iso found under $iso_path/iso"

printf '\n:: ISO built:\n   %s\n' "$iso_file"
ls -lh "$iso_file"

$flash || exit 0

# --- 2. Resolve the active root disk (for safety check) -----------------------

# Walk up from the root filesystem's source to its underlying physical disk.
# Handles LUKS (/dev/mapper/crypt-root -> /dev/nvme0n1p2 -> /dev/nvme0n1).
root_source="$(findmnt -no SOURCE / 2>/dev/null || true)"
root_disk=""
if [[ -n "$root_source" ]]; then
  # PKNAME on a mapper returns empty; recurse through partitions.
  current="$root_source"
  for _ in 1 2 3; do
    pk="$(lsblk -ndo PKNAME "$current" 2>/dev/null || true)"
    [[ -n "$pk" ]] || break
    current="/dev/$pk"
  done
  root_disk="$(readlink -f "$current" 2>/dev/null || true)"
fi

# --- 3. Disk selection via fzf ------------------------------------------------

fzf_bin="${FZF:-fzf}"
command -v "$fzf_bin" >/dev/null 2>&1 ||
  die "fzf not found in PATH (set FZF to override)"

# Build a human-readable disk listing for fzf: path, name, size, type, mounts.
disk_lines="$(lsblk -npo NAME,MODEL,SIZE,TYPE,MOUNTPOINTS | awk -F'\t' '$4 == "disk" { print }')"

if [[ -n "$preselected_device" ]]; then
  selected="$preselected_device"
  printf ':: Using preselected device: %s\n' "$selected"
else
  printf '\n:: Select a target disk (all disks shown):\n\n'
  # fzf prints the full first field (NAME) of the chosen line.
  selected="$(printf '%s\n' "$disk_lines" | $fzf_bin \
    --prompt='disk> ' \
    --header='Tab to multi-mark is disabled; pick ONE whole disk' \
    --no-multi \
    --height=40% \
    --reverse \
    --preview="lsblk -o NAME,SIZE,TYPE,FSTYPE,MOUNTPOINTS {1}" \
    --preview-window=right:50%:wrap \
    | awk '{print $1}')"
  [[ -n "$selected" ]] || die "no disk selected"
fi

selected="$(readlink -f "$selected")"
[[ -b "$selected" ]] || die "not a block device: $selected"
[[ "$(lsblk -ndo TYPE "$selected")" == "disk" ]] ||
  die "pass the whole disk, not a partition: $selected"

# --- 4. Safety: refuse to wipe disks that host mounts or the active root ------

mountpoints_on_disk="$(lsblk -nrpo MOUNTPOINTS "$selected" 2>/dev/null || true)"
if [[ "$mountpoints_on_disk" =~ [^[:space:]] ]]; then
  printf '\n%s: REFUSING to write to %s — it has mounted filesystems:\n' "$PROG" "$selected"
  lsblk -o NAME,SIZE,TYPE,FSTYPE,MOUNTPOINTS "$selected" >&2
  printf '\nUnmount them first, or pick a different disk.\n' >&2
  die "target disk has active mounts"
fi

if [[ -n "$root_disk" && "$selected" == "$root_disk" ]]; then
  printf '\n%s: REFUSING to write to %s — that disk hosts the active root (%s).\n' \
    "$PROG" "$selected" "$root_source" >&2
  die "target disk is the active root disk"
fi

# --- 5. Three escalating confirmation prompts ---------------------------------

printf '\n'
printf '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'
printf '!!  ABOUT TO OVERWRITE %s WITH:                          !!\n' "$selected"
printf '!!  %s\n' "$iso_file"
printf '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'
printf '\n'

printf 'This will destroy all data on %s. There is no undo.\n' "$selected"
read -r -p "Type the device path to continue (1/3): " answer
[[ "$answer" == "$selected" ]] ||
  die "confirmation 1 failed: expected %s, got %s" "$selected" "$answer"

printf '\nFinal warning. This writes raw bytes to the disk and wipes the\n'
printf 'partition table, all filesystems, and all data on %s.\n' "$selected"
read -r -p "Type WIPE-DISK to continue (2/3): " answer
[[ "$answer" == "WIPE-DISK" ]] || die "confirmation 2 failed: expected WIPE-DISK"

printf '\nLast chance. After this, dd runs to completion.\n'
read -r -p "Type YES-I-AM-SURE to begin (3/3): " answer
[[ "$answer" == "YES-I-AM-SURE" ]] ||
  die "confirmation 3 failed: expected YES-I-AM-SURE"

# --- 6. dd the ISO ------------------------------------------------------------
# dd needs root; nix build does not. We escalate only for the write step so
# `nix run .#flash-logos` works without `sudo nix run` (which fails because
# nix isn't in root's PATH).

printf '\n:: Writing %s -> %s\n' "$iso_file" "$selected"

if $dry_run; then
  printf ':: [dry-run] would run: sudo dd if=%s of=%s bs=4M oflag=direct,sync conv=fsync status=progress\n' \
    "$iso_file" "$selected"
  exit 0
fi

# If we're already root, run dd directly. Otherwise escalate via sudo.
if [[ $EUID -eq 0 ]]; then
  dd_cmd=(dd if="$iso_file" of="$selected" bs=4M oflag=direct,sync conv=fsync status=progress)
else
  dd_cmd=(sudo dd if="$iso_file" of="$selected" bs=4M oflag=direct,sync conv=fsync status=progress)
fi

# oflag=direct: bypass page cache for honest progress.
# oflag=sync:  flush writes at the end.
# conv=fsync:  also flush metadata.
# bs=4M:       large block size for throughput on USB/NVMe.
"${dd_cmd[@]}"

# Make sure the kernel re-reads the new partition table.
sync
if [[ $EUID -eq 0 ]]; then
  partprobe "$selected" 2>/dev/null || true
else
  sudo partprobe "$selected" 2>/dev/null || true
fi

printf '\n:: Done. ISO written to %s.\n' "$selected"
printf ':: You can now boot from %s (it will appear as a USB/NVMe device in your firmware boot menu).\n' \
  "$selected"
