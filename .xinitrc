#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for script in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -r "$script" ] && . "$script"
    done
    unset script
fi

export XDG_CURRENT_DESKTOP=Qtile
export XDG_SESSION_DESKTOP=qtile
export DESKTOP_SESSION=qtile

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
    dbus-update-activation-environment --systemd \
        DISPLAY \
        XAUTHORITY \
        XDG_CURRENT_DESKTOP \
        XDG_SESSION_DESKTOP \
        DESKTOP_SESSION
fi

if command -v gnome-keyring-daemon >/dev/null 2>&1; then
    gnome-keyring-daemon --start --components=secrets
fi

exec qtile start
