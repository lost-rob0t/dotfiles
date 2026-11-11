#!/usr/bin/env bash
# Bump the vendored Brave package to a new upstream release.
# Usage: ./update.sh <version>   e.g. ./update.sh 1.95.110
set -euo pipefail

version="${1:?usage: update.sh <version>}"
dir="$(cd "$(dirname "$0")" && pwd)"

amd64=$(nix-prefetch-url --type sha256 "https://github.com/brave/brave-browser/releases/download/v${version}/brave-browser_${version}_amd64.deb")
arm64=$(nix-prefetch-url --type sha256 "https://github.com/brave/brave-browser/releases/download/v${version}/brave-browser_${version}_arm64.deb")
amd64_sri=$(nix hash convert --to sri --hash-algo sha256 "$amd64")
arm64_sri=$(nix hash convert --to sri --hash-algo sha256 "$arm64")

cat > "$dir/release.nix" <<EOF
# Expression generated for easy version bumps; update version and hashes
# together from the upstream release assets.
rec {
  pname = "brave";
  version = "${version}";

  archives = {
    x86_64-linux = {
      url = "https://github.com/brave/brave-browser/releases/download/v\${version}/brave-browser_\${version}_amd64.deb";
      sha256 = "${amd64_sri}";
    };
    aarch64-linux = {
      url = "https://github.com/brave/brave-browser/releases/download/v\${version}/brave-browser_\${version}_arm64.deb";
      sha256 = "${arm64_sri}";
    };
  };
}
EOF

echo "brave ${version} written to release.nix"
