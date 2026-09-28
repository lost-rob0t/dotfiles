#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

for script in scripts/lish scripts/lish-bootstrap scripts/lish-default-shell scripts/lish-rlm scripts/lish-expert scripts/lish-bash-enter; do
  bash -n "$script"
done

if command -v shellcheck >/dev/null 2>&1; then
  shellcheck --severity=warning \
    scripts/lish scripts/lish-bootstrap scripts/lish-default-shell scripts/lish-rlm scripts/lish-expert scripts/lish-bash-enter
fi

grep -Fq '7463781d04c8e35db22bb88796dd78c1a8f752bd' scripts/lish-bootstrap
grep -Fq 'ae3715d74548e451bb08b432c5d25ec93e2a02c1' scripts/lish-bootstrap
grep -Fq '4715b5ee53d0e7a26c55705ea85c0d16c3916504' scripts/lish-bootstrap

grep -Fq 'STAR_BASH_FALLBACK' scripts/lish-bash-enter
grep -Fq 'STAR_LISH_ACTIVE' scripts/lish-bash-enter
grep -Fq 'STAR_LISH_DEFAULT_INTERACTIVE' scripts/lish-bash-enter
grep -Fq 'lish-default-shell <status|install|restore>' scripts/lish-default-shell
grep -Fq -- '-l|--login' scripts/lish
grep -Fq 'star-enable-agentic-evaluator' .config/lish/agentic.lisp
grep -Fq 'lish:defcommand agent' .config/lish/agentic.lisp
grep -Fq 'lish:defcommand experts' .config/lish/agentic.lisp
grep -Fq 'symbolic-recursive' .config/lish/agentic.lisp
grep -Fq './lish.nix' nix/home-manager/mods/default.nix
grep -Fq 'lish.enable = true;' nix/home-manager/systems/desktop/home.nix
grep -Fq 'scripts/lish-bash-enter' bash.org
grep -Fq 'scripts/lish-bash-enter' .bashrc
grep -Fq 'Installing Lish agentic shell' bootstrap-termux.org
grep -Fq 'Installing Lish agentic shell' bootstrap-termux.sh

printf 'lish-shell: PASS\n'
