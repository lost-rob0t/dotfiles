#!/usr/bin/env bash

search_nixpkgs() {
    local search_term="$1"
    local json_output=false
    local max_results=20  # Limit number of results

    # Check if --json flag is present
    if [[ "$1" == "--json" ]]; then
        json_output=true
        search_term="$2"
    fi

    if [[ -z "$search_term" ]]; then
        echo "Usage: search_nixpkgs [--json] SEARCH_TERM"
        echo "For packages: search_nixpkgs 'firefox'"
        echo "For services/options: search_nixpkgs 'services.'"
        return 1
    fi

    # If the search term contains a dot, assume it's a NixOS option search
    if [[ "$search_term" == *"."* ]]; then
        echo "Searching NixOS options (timeout: 10s)..."
        if [[ "$json_output" == true ]]; then
            timeout 10 nix eval --json "nixpkgs#nixosConfigurations.dummy.options.$search_term" 2>/dev/null || \
            timeout 10 nix-instantiate --eval -E "with import <nixpkgs/nixos> { configuration = {}; }; config.$search_term" --json
        else
            timeout 10 nix eval "nixpkgs#nixosConfigurations.dummy.options.$search_term" 2>/dev/null || \
            timeout 10 nix-instantiate --eval -E "with import <nixpkgs/nixos> { configuration = {}; }; config.$search_term"
        fi
    else
        echo "Searching nixpkgs packages (timeout: 10s)..."
        if [[ "$json_output" == true ]]; then
            timeout 10 nix search nixpkgs "$search_term" --json | head -c 50000
        else
            timeout 10 nix search nixpkgs "$search_term" | head -n "$max_results"
        fi
    fi
}