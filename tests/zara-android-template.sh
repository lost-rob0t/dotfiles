#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
manifest="$repo_root/zara-template.properties"
org="$repo_root/docs/wiki/zara-android-automation.org"

die() {
    printf 'zara android template: %s\n' "$*" >&2
    exit 1
}

[[ -f "$manifest" ]] || die "missing zara-template.properties"
[[ -f "$org" ]] || die "missing Org template"
grep -qx 'version=1' "$manifest" || die "template version must be 1"
grep -qx 'name=nsaspy-dotfiles-zara-android' "$manifest" || die "unexpected template name"
grep -qx 'org=docs/wiki/zara-android-automation.org' "$manifest" || die "manifest does not point at canonical Org file"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

awk '
    BEGIN { in_block=0; found=0 }
    /^#\+begin_src prolog :tangle android_automation\.pl$/ {
        if (in_block) exit 2
        in_block=1
        found++
        next
    }
    /^#\+end_src$/ && in_block {
        in_block=0
        next
    }
    in_block { print }
    END {
        if (in_block || found != 1) exit 3
    }
' "$org" > "$tmp/android_automation.pl" || die "invalid android_automation.pl Org tangle"

[[ -s "$tmp/android_automation.pl" ]] || die "tangled Prolog source is empty"
swipl -q -t halt -s "$tmp/android_automation.pl"

grep -q "automation(youtube_psytrance" "$tmp/android_automation.pl" || die "missing YouTube demo"
grep -q "app_search(youtube, 'psytrance')" "$tmp/android_automation.pl" || die "missing YouTube psytrance search"
grep -q "automation(revanced_psytrance" "$tmp/android_automation.pl" || die "missing ReVanced demo"
grep -q "app_search(youtube_revanced, 'psytrance')" "$tmp/android_automation.pl" || die "missing ReVanced psytrance search"

if grep -Eq '(com\.google\.android\.youtube|app\.revanced\.android\.youtube|android\.intent|shell\(|process_create)' "$tmp/android_automation.pl"; then
    die "portable template contains platform package/intent/shell authority"
fi

printf 'zara android config template contract: PASS\n'
