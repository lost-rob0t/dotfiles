% distro/config.pl -- release policy for the dotfiles meta-distro.
%
% This is the human-owned policy file.  Agents do not weaken it to make a
% gate pass; the human edits it deliberately.

% Council: how many distinct councilors must approve (last vote per
% councilor wins; any standing reject blocks).
council_quorum(2).

% Adversarial "attack" suite: repo-root-relative shell commands that must
% all exit 0 at the current epoch before a release gate can pass.
attack_step('scripts/check-literate-sync').
attack_step('tests/distro-test.sh').
