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

# Qtile talks to Xorg over the X socket, not the controlling tty. startx
# keeps the foreground process group of tty1, so qtile and everything it
# spawns (including Electron apps that re-open /dev/tty) would run as a
# background pgrp and get stopped by SIGTTIN/SIGTSTP on any tty read,
# freezing the whole WM.
#
# `setsid` puts qtile in a new session with no controlling terminal, so
# /dev/tty is unopenable (ENXIO) by qtile or its children. Stdio is pointed
# at /dev/null and a session log; VT switching still works because Xorg owns
# the vt, not qtile.
session_log="$HOME/.local/state/qtile/session.log"
mkdir -p "$(dirname "$session_log")"
exec setsid qtile start </dev/null >>"$session_log" 2>&1
