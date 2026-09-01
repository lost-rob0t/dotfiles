#!/usr/bin/env bash

# Idempotent launcher: start "$@" only if it is not already running as
# recorded by a per-command PID file. Safe to call repeatedly (qtile reload),
# so this whole script can be re-run on every qtile restart without spawning
# duplicates.
#
# Why PID files instead of pgrep:
#   - pgrep -x matches the 15-char truncated /proc/<pid>/comm, so it cannot
#     match long names like polkit-gnome-authentication-agent-1 and cannot
#     distinguish multiple instances of the same binary with different
#     arguments.
#   - pgrep -f matches the full command line, but long-running daemons
#     (conky, brave, ...) rewrite or shorten /proc/<pid>/cmdline after
#     daemonizing, so the pattern no longer matches and duplicates get
#     launched.
# A PID file we control is immune to both problems.

_state_dir="${XDG_RUNTIME_DIR:-/tmp}/qtile-autostart"
mkdir -p "$_state_dir"

# Normalise a command string into a safe, flat filename component.
# Slashes are stripped too so the result is a single filename, not a path.
_key() {
    printf '%s' "$*" | tr -s ' \t\n/' '_' | tr -c 'A-Za-z0-9._-' '_'
}

# Is the given PID alive AND running the expected program?
# Compares /proc/<pid>/cmdline (first token, the binary) to the first argument
# we were handed, so a stale PID reused by an unrelated process is rejected.
_alive() {
    local pid="$1" bin="$2"
    [ -n "$pid" ] && [ -n "$bin" ] || return 1
    [ -d "/proc/$pid" ] || return 1
    local cur
    cur=$(tr '\0' ' ' </proc/"$pid"/cmdline 2>/dev/null | cut -d' ' -f1)
    # Resolve both to absolute paths so "conky" matches "/usr/bin/conky".
    local want real
    want=$(command -v "$bin" 2>/dev/null || printf '%s' "$bin")
    real=$(command -v "$cur"  2>/dev/null || printf '%s' "$cur")
    [ "$want" = "$real" ]
}

# run <program> [args...]
function run() {
    local bin="$1"
    [ -n "$bin" ] || return 0
    local key pidfile
    key="$(_key "$*")"
    pidfile="$_state_dir/$key.pid"

    if [ -f "$pidfile" ]; then
        local old
        old=$(cat "$pidfile" 2>/dev/null)
        if _alive "$old" "$bin"; then
            # Already running as expected; do nothing.
            return 0
        fi
        # Stale PID file; reclaim it.
        rm -f "$pidfile"
    fi

    # Launch and record the PID. Use setsid so the child survives this shell
    # exiting and does not keep our tty open.
    setsid "$@" </dev/null >/dev/null 2>&1 &
    local new=$!
    printf '%s\n' "$new" >"$pidfile"
}

# Set your native resolution IF it does not exist in xrandr
# More info in the script
#run $HOME/.config/qtile/scripts/set-screen-resolution-in-virtualbox.sh

# Find out your monitor name with xrandr or arandr (save and you get this line)
# xrandr --output VGA-1 --primary --mode 1360x768 --pos 0x0 --rotate normal
# xrandr --output DP2 --primary --mode 1920x1080 --rate 60.00 --output LVDS1 --off
# xrandr --output LVDS1 --mode 1366x768 --output DP3 --mode 1920x1080 --right-of LVDS1
# xrandr --output HDMI2 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off
# autorandr horizontal

# change your keyboard if you need it
# setxkbmap -layout be

# One-shot, idempotent: overwrites config.py only on azerty layouts.
keybLayout=$(setxkbmap -v | awk -F "+" '/symbols/ {print $2}')

if [ "$keybLayout" = "be" ]; then
    cp "$HOME/.config/qtile/config-azerty.py" "$HOME/.config/qtile/config.py"
fi

# Some ways to set your wallpaper besides variety or nitrogen
# feh --bg-fill /usr/share/backgrounds/archlinux-tweak-tool/data/wallpaper/wallpaper.png &

# Monitor-aware Conky deck. The launcher owns per-head PIDs and assigns
# SYSTEM/AI/WORK/NET across however many active monitors are present.
"$HOME/.local/bin/conky-rotate" --all >/dev/null 2>&1 || true

# Qtile popups use a separate Emacs server so the user's interactive Emacs is
# never taken over. Qtile has already inherited DISPLAY and XAUTHORITY here,
# so popup clients can create frames on the live X display.
start_qtile_emacs_server() {
    if command -v emacsclient >/dev/null 2>&1 && \
        emacsclient -s qtile -a false --eval t >/dev/null 2>&1; then
        return 0
    fi
    command -v emacs >/dev/null 2>&1 || return 0
    setsid emacs --daemon=qtile >/dev/null 2>&1 &
}

start_qtile_emacs_server

# starting utility applications at boot time
run variety
run nm-applet
# run pamac-tray
run xfce4-power-manager
# One-shot, idempotent: re-asserts numlock state. Not a daemon, so not run()-guarded.
# Guarded so a host without numlockx doesn't noisy-fail on every reload.
command -v numlockx >/dev/null 2>&1 && numlockx on
run blueman-applet
run picom
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run /usr/lib/xfce4/notifyd/xfce4-notifyd
run spice-vdagent
# starting user applications at boot time
run volumeicon
run /home/unseen/.bin/graphics.sh
run brave
run python3 /home/unseen/.dotfiles/.config/qtile/scripts/pinger.py -c "$HOME/.config/hosts.toml"
run kdeconnect-indicator
run aw-qt
