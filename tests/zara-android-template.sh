#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
manifest="$repo_root/zara-template.properties"
config_dir="$repo_root/.config/zarathushtra/android"
automation="$config_dir/android_automation.pl"

die() {
    printf 'zara android template: %s\n' "$*" >&2
    exit 1
}

[[ -f "$manifest" ]] || die "missing zara-template.properties"
[[ -d "$config_dir" ]] || die "missing canonical Android Zara config directory"
[[ -f "$automation" ]] || die "missing Android automation Prolog source"

grep -qx 'version=1' "$manifest" || die "template version must be 1"
grep -qx 'name=nsaspy-dotfiles-zara-android' "$manifest" || die "unexpected template name"
grep -qx 'directory=.config/zarathushtra/android' "$manifest" || die "manifest does not point at canonical XDG Android config"

find "$config_dir" -mindepth 1 -maxdepth 1 -type f -name '*.pl' -print0 |
while IFS= read -r -d '' source; do
    swipl -q -t halt -s "$source"
done

grep -q "automation(youtube_psytrance" "$automation" || die "missing YouTube demo"
grep -q 'app_search(youtube, "psytrance")' "$automation" || die "missing typed YouTube psytrance search"
grep -q "automation(revanced_psytrance" "$automation" || die "missing ReVanced demo"
grep -q 'app_search(youtube_revanced, "psytrance")' "$automation" || die "missing typed ReVanced psytrance search"

if grep -Eq '(com\.google\.android\.youtube|app\.revanced\.android\.youtube|android\.intent|shell\(|process_create)' "$automation"; then
    die "portable Android config contains platform package/intent/shell authority"
fi

[[ ! -e "$repo_root/.config/zarathushtra/config.local.pl" ]] ||
    die "private config.local.pl must not be tracked in the dotfiles repository"

printf 'zara android config template contract: PASS\n'
