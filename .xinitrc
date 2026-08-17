#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for script in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -r "$script" ] && . "$script"
    done
    unset script
fi

export XDG_SESSION_TYPE=x11
export XDG_CURRENT_DESKTOP=qtile
export XDG_SESSION_DESKTOP=qtile
export DESKTOP_SESSION=qtile

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
    dbus-update-activation-environment --systemd \
        DISPLAY \
        XAUTHORITY \
        XDG_SESSION_TYPE \
        XDG_CURRENT_DESKTOP \
        XDG_SESSION_DESKTOP \
        DESKTOP_SESSION
fi

systemctl --user import-environment \
    DISPLAY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP DESKTOP_SESSION

if command -v gnome-keyring-daemon >/dev/null 2>&1; then
    gnome-keyring-daemon --start --components=secrets
fi

"$HOME/.config/qtile/scripts/setup-monitors.sh"

# Qtile talks to Xorg over the X socket, not the controlling tty. Keep stdio
# off /dev/tty1: startx retains the foreground process group, so any read of
# tty1 by qtile or its children (SIGTTIN) would stop the whole WM.
session_log="$HOME/.local/state/qtile/session.log"
mkdir -p "$(dirname "$session_log")"
exec qtile start </dev/null >>"$session_log" 2>&1
