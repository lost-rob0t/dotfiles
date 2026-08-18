#!/usr/bin/env bash

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi

# Synchronize a git checkout with its configured upstream without creating
# merge commits or rewriting local history. Defaults to the current directory.
git-sync() {
    local repo="${1:-$PWD}"
    local upstream

    if ! git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        printf 'git-sync: not a git repository: %s\n' "$repo" >&2
        return 2
    fi

    upstream="$(git -C "$repo" rev-parse --abbrev-ref --symbolic-full-name '@{upstream}' 2>/dev/null)" || {
        printf 'git-sync: no upstream configured for %s\n' "$repo" >&2
        return 3
    }

    git -C "$repo" fetch --prune || return
    git -C "$repo" merge --ff-only "$upstream"
}
export -f git-sync

# Start the X11 Qtile session after logging in on the first virtual terminal.
if [[ -z "${DISPLAY:-}" && -z "${WAYLAND_DISPLAY:-}" ]]; then
    case "$(tty)" in
        /dev/tty1)
            exec startx
            ;;
    esac
fi
