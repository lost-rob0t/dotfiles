#!/usr/bin/env bash

# Synchronize a git checkout with its configured upstream without creating
# merge commits or rewriting local history. Defaults to the current directory.
#
# This file intentionally supports both entry points:
# - source it from Bash to define the git-sync function;
# - execute it (or package it) to use git-sync as a normal command.
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

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    git-sync "$@"
fi
