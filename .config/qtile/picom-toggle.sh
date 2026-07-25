#!/usr/bin/env bash
set -euo pipefail

if pgrep -x picom >/dev/null; then
    pkill -x picom
else
    picom -b --config "$HOME/.config/picom/picom.conf"
fi
