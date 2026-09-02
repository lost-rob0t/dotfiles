#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
module="$repo_root/nix/home-manager/mods/desktop.nix"

die() {
  printf 'variety-home-manager: %s\n' "$*" >&2
  exit 1
}

if grep -Fq 'home.file.".config/variety/scripts/set_qtile.sh"' "$module"; then
  die 'Variety runtime script must not be managed with home.file'
fi

grep -Fq 'home.activation.varietyQtileScript' "$module" \
  || die 'missing Variety activation installer'
grep -Fq 'qtileWallpaperScript = pkgs.writeText' "$module" \
  || die 'missing declarative Variety Qtile script source'
grep -Fq '${pkgs.coreutils}/bin/rm -f "$target"' "$module" \
  || die 'activation must remove an old Home Manager symlink before install'
grep -Fq '${pkgs.coreutils}/bin/install -Dm700' "$module" \
  || die 'activation must install a private writable executable copy'

workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT
source_script="$workdir/store-script"
target="$workdir/config/variety/scripts/set_qtile.sh"

mkdir -p "$(dirname "$target")"
printf '#!/bin/sh\nexit 0\n' > "$source_script"
chmod 0555 "$source_script"
ln -s "$source_script" "$target"

rm -f "$target"
install -Dm700 "$source_script" "$target"

[[ ! -L "$target" ]] || die 'installed runtime script is still a symlink'
[[ -w "$target" ]] || die 'installed runtime script is not owner-writable'
[[ -x "$target" ]] || die 'installed runtime script is not executable'

printf 'variety-home-manager: ok\n'
