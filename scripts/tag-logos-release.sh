#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: tag-logos-release.sh [--yes] [--dry-run] [--remote NAME]

Creates and pushes the next available date-based Logos release tag:
  logos-YYYY.MM.DD
  logos-YYYY.MM.DD.2
  logos-YYYY.MM.DD.3

Environment:
  LOGOS_RELEASE_DATE=YYYY.MM.DD  Override the date, mainly for testing.
EOF
}

remote="origin"
assume_yes=false
dry_run=false

while (($#)); do
  case "$1" in
    --yes|-y)
      assume_yes=true
      ;;
    --dry-run|-n)
      dry_run=true
      ;;
    --remote)
      shift
      [[ $# -gt 0 ]] || { echo "--remote requires a value" >&2; exit 2; }
      remote="$1"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

repo_root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "Run this inside the dotfiles Git repository." >&2
  exit 1
}
cd "$repo_root"

if [[ -n "$(git status --porcelain)" ]]; then
  echo "Working tree is not clean. Commit or stash changes before releasing." >&2
  exit 1
fi

git remote get-url "$remote" >/dev/null 2>&1 || {
  echo "Git remote '$remote' does not exist." >&2
  exit 1
}

git fetch --quiet --tags "$remote"

release_date="${LOGOS_RELEASE_DATE:-$(date +%Y.%m.%d)}"
[[ "$release_date" =~ ^[0-9]{4}\.[0-9]{2}\.[0-9]{2}$ ]] || {
  echo "Invalid release date '$release_date'; expected YYYY.MM.DD." >&2
  exit 1
}

base_tag="logos-${release_date}"
tag="$base_tag"
sequence=2

while git rev-parse --quiet --verify "refs/tags/$tag" >/dev/null; do
  tag="${base_tag}.${sequence}"
  sequence=$((sequence + 1))
done

commit="$(git rev-parse HEAD)"
printf 'Release tag: %s\nCommit:      %s\nRemote:      %s\n' "$tag" "$commit" "$remote"

if $dry_run; then
  exit 0
fi

if ! $assume_yes; then
  read -r -p "Create and push this tag? [y/N] " answer
  [[ "$answer" =~ ^[Yy]$ ]] || {
    echo "Cancelled."
    exit 1
  }
fi

git tag --annotate "$tag" --message "Logos NixOS installer release $tag"
git push "$remote" "refs/tags/$tag"

echo "Pushed $tag. GitHub Actions will build and publish the ISO under Releases."
