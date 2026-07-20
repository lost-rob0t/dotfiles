#!/usr/bin/env bash
set -Eeuo pipefail

fail_dialog() {
  zenity --error --title="Logos Installer" --text="$1" >/dev/null 2>&1 || true
  exit 1
}

command -v zenity >/dev/null || {
  printf 'install-logos-gui: zenity is required\n' >&2
  exit 1
}

mapfile -t disk_rows < <(
  lsblk --json --bytes --nodeps --paths --output NAME,SIZE,MODEL,TYPE |
    jq -r '.blockdevices[] | select(.type == "disk") | [.name, (.size | tostring), (.model // "Unknown")] | @tsv'
)

[[ ${#disk_rows[@]} -gt 0 ]] || fail_dialog "No whole disks were detected."

list_args=()
for row in "${disk_rows[@]}"; do
  IFS=$'\t' read -r device size_bytes model <<< "$row"
  size="$(numfmt --to=iec-i --suffix=B "$size_bytes")"
  list_args+=(FALSE "$device" "$size" "$model")
done

selected_disk="$(
  zenity \
    --list \
    --radiolist \
    --title="Install Logos" \
    --width=760 \
    --height=420 \
    --text="Select the entire disk to erase and install Logos onto." \
    --column="Select" \
    --column="Device" \
    --column="Size" \
    --column="Model" \
    --print-column=2 \
    "${list_args[@]}"
)" || exit 0

[[ -n "$selected_disk" ]] || exit 0

flake="${LOGOS_DEFAULT_FLAKE:-/etc/logos#logos}"
flake="$(
  zenity \
    --entry \
    --title="Logos Flake" \
    --text="Flake configuration to install:" \
    --entry-text="$flake"
)" || exit 0

[[ "$flake" == *#* ]] || fail_dialog "The flake must include a configuration fragment, such as /etc/logos#logos."

password="$(zenity --password --title="Logos User Password" --text="Password for the unseen user:")" || exit 0
[[ -n "$password" ]] || fail_dialog "The password cannot be empty."

password_confirmation="$(zenity --password --title="Confirm Password" --text="Enter the unseen user password again:")" || exit 0
[[ "$password" == "$password_confirmation" ]] || fail_dialog "The passwords do not match."
unset password_confirmation

summary="Target disk: $selected_disk
Flake: $flake

ALL DATA ON THE TARGET DISK WILL BE DESTROYED.
The installer will create LUKS2 encryption and Btrfs subvolumes."

zenity \
  --question \
  --title="Confirm Logos Installation" \
  --width=620 \
  --ok-label="Erase Disk and Install" \
  --cancel-label="Cancel" \
  --text="$summary" || exit 0

password_file="$(mktemp --tmpdir="${XDG_RUNTIME_DIR:-/tmp}" logos-password.XXXXXX)"
trap 'rm -f -- "$password_file"' EXIT
chmod 600 "$password_file"
printf '%s\n' "$password" > "$password_file"
unset password

install_command=(
  sudo
  install-logos
  --disk "$selected_disk"
  --flake "$flake"
  --password-file "$password_file"
  --yes
)

printf -v quoted_command '%q ' "${install_command[@]}"
terminal_body="$quoted_command; status=\$?; printf '\\n'; if (( status == 0 )); then echo 'Logos installation completed.'; else echo \"Logos installation failed with status \$status.\"; fi; read -r -p 'Press Enter to close...'; exit \$status"

if qterminal -e bash -lc "$terminal_body"; then
  zenity --info --title="Logos Installer" --text="Installation completed. You can reboot into Logos." >/dev/null 2>&1 || true
else
  zenity --error --title="Logos Installer" --text="Installation failed. Review the terminal output." >/dev/null 2>&1 || true
  exit 1
fi
