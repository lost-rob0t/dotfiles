#!/usr/bin/env bash

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi

# Start the X11 Qtile session after logging in on the first virtual terminal.
if [[ -z "${DISPLAY:-}" && -z "${WAYLAND_DISPLAY:-}" ]]; then
    case "$(tty)" in
        /dev/tty1)
            exec startx
            ;;
    esac
fi
