#!/usr/bin/env bash
# Emits K=V observation facts for the bluetooth-audio expert pack.
# Read-only, no root required.
#
# Overrides:
#   BLUETOOTH_DEV_MAC   headset MAC   (default 20:AF:1B:1F:12:26)
#   BLUETOOTH_USB_VENDOR / BLUETOOTH_USB_PRODUCT  radio ids (0bda / a729)
set -u

DEV_MAC="${BLUETOOTH_DEV_MAC:-20:AF:1B:1F:12:26}"
MAC_COL="${DEV_MAC//:/_}"
VENDOR="${BLUETOOTH_USB_VENDOR:-0bda}"
PRODUCT="${BLUETOOTH_USB_PRODUCT:-a729}"

# --- live A2DP codec (first bluez sink node) ---
codec=$(pw-dump 2>/dev/null | grep -o '"api.bluez5.codec": "[^"]*"' | head -1 \
        | sed 's/.*: "//; s/"$//')
echo "codec=${codec:-none}"

# --- bluez card profiles ---
card_dump=$(pactl list cards 2>/dev/null | awk '/bluez_card/{f=1} f')
profile=$(printf '%s\n' "$card_dump" | grep 'Active Profile' \
          | sed 's/.*:[[:space:]]*//' | tr '-' '_')
echo "active_profile=${profile:-none}"
if printf '%s\n' "$card_dump" | grep -q 'a2dp-sink:'; then
    echo "a2dp_profile_available=yes"
else
    echo "a2dp_profile_available=no"
fi

# --- journal evidence since the current bluetoothd / wireplumber start ---
# (stale-SEP state and transport flaps are per-process-generation evidence)
btd_since=$(systemctl show bluetooth -p ActiveEnterTimestamp --value 2>/dev/null)
wp_since=$(systemctl --user show wireplumber -p ActiveEnterTimestamp --value 2>/dev/null)
btd=$(journalctl -b -u bluetooth ${btd_since:+--since "$btd_since"} --no-pager 2>/dev/null || true)
wp=$(journalctl --user -b -u wireplumber ${wp_since:+--since "$wp_since"} --no-pager 2>/dev/null || true)
echo "stale_sep_errors=$(printf '%s\n' "$btd" | grep -c 'load_remote_sep()' || true)"
echo "a2dp_connect_failures=$(printf '%s\n' "$btd" \
     | grep -c 'a2dp-sink profile connect failed' || true)"
echo "transport_failures=$(printf '%s\n%s\n' "$btd" "$wp" \
     | grep -c 'Failure in Bluetooth audio transport' || true)"
echo "completion_report_errors=$(printf '%s\n' "$wp" \
     | grep -c 'Missing completion reports' || true)"

# --- persistent fixes present? ---
[ -f /etc/udev/rules.d/85-btusb-no-autosuspend.rules ] \
    && echo "udev_rule_present=yes" || echo "udev_rule_present=no"
[ -f /etc/modprobe.d/btusb-no-autosuspend.conf ] \
    && echo "modprobe_conf_present=yes" || echo "modprobe_conf_present=no"
[ -f "$HOME/.config/wireplumber/wireplumber.conf.d/50-bluetooth-quality.conf" ] \
    && echo "wp_dropin_present=yes" || echo "wp_dropin_present=no"

# --- USB autosuspend state of the radio (normalized) ---
# sysfs power/control: "auto" = autosuspend ENABLED (hazard),
# "on" = device stays powered = autosuspend DISABLED (good).
autosuspend=unknown
for idpath in /sys/bus/usb/devices/*/idVendor; do
    dev_dir=${idpath%idVendor}
    [ "$(cat "$dev_dir/idVendor" 2>/dev/null)" = "$VENDOR" ] || continue
    [ "$(cat "$dev_dir/idProduct" 2>/dev/null)" = "$PRODUCT" ] || continue
    case "$(cat "$dev_dir/power/control" 2>/dev/null || echo unknown)" in
        auto) autosuspend=enabled ;;
        on)   autosuspend=disabled ;;
        *)    autosuspend=unknown ;;
    esac
    break
done
echo "usb_autosuspend=${autosuspend}"

% --- fresh flap evidence (last 15 minutes only) ---
btd_r=$(journalctl -b -u bluetooth --since "-15 min" --no-pager 2>/dev/null || true)
wp_r=$(journalctl --user -b -u wireplumber --since "-15 min" --no-pager 2>/dev/null || true)
echo "transport_failures_recent=$(printf '%s\n%s\n' "$btd_r" "$wp_r" \
     | grep -c 'Failure in Bluetooth audio transport' || true)"
echo "completion_report_errors_recent=$(printf '%s\n' "$wp_r" \
     | grep -c 'Missing completion reports' || true)"
