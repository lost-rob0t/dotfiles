% nixpkgs removed sonixd 2026-10-30 (archived upstream, unmaintained); the
% alias now throws. Replacement is feishin (rewrite by the original author).
% Zara had already been migrated to feishin app mappings (.config/zarathushtra/
% config.pl, tests/zara-workflows.sh); media.nix was the last consumer and
% swapped to feishin in the 2026-11-25 input update (deployed gen 172).
removed_package(sonixd, '2026-10-30', feishin).
% The codex pkg-config override (zstd/aws-lc/oniguruma/sqlite buildInputs) was
% dropped in the same update: nixpkgs codex 0.171.0 vendors the native probe
% inputs itself, Hydra builds it with the same toolchain, and the override
% only forced a local 650MiB Rust rebuild. Kept as a dated flake.nix comment.
dropped_override(codex_pkgconfig_inputs, 'nixpkgs codex 0.171.0 (2026-11-25 bump)').

% nixpkgs deprecation mappings enforced by tests/nix-deprecations.sh.

:- discontiguous deprecated_attr/2.
:- discontiguous replacement/2.

deprecated_attr(pkgs_system, '${pkgs.system}').
replacement(pkgs_system, '${pkgs.stdenv.hostPlatform.system}').

deprecated_attr(stdenv_is_linux, 'stdenv.isLinux').
replacement(stdenv_is_linux, 'stdenv.hostPlatform.isLinux').

deprecated_attr(xorg_xrandr, 'xorg.xrandr').
replacement(xorg_xrandr, 'xrandr').

deprecated_attr(xorg_xinit, 'xorg.xinit').
replacement(xorg_xinit, 'xinit').

deprecated_attr(input_default_package, 'inputs.<name>.defaultPackage.<system>').
replacement(input_default_package, 'inputs.<name>.packages.<system>.default').

% Fixed in this repo by fix/hm-qwen3-clobber:
%   nix/home-manager/systems/desktop/programs.nix (pkgs.system)
%   nix/home-manager/mods/lisp.nix (stdenv.isLinux)
%   nix/home-manager/mods/screen-capture.nix (xorg.xrandr)
%   nix/nixos/systems/flake/packages.nix (xorg.xinit)

% The Mousetrap flake now exposes packages.<system>.default; the gaming
% profile consumes that output without changing the derivation.
consumer_package_output(mousetrap, 'packages.x86_64-linux.default').

% Warnings NOT fixable in this repository (upstream owned):
upstream_deprecation(qwen3_tts_module_pkgs_system,
    'lost-rob0t/Qwen3-TTS_server nix/home-manager-module.nix uses pkgs.system').
% Verified 2026-09-12 by forcing home.packages outPaths one by one:
% all 14 xorg lib rename warnings come from the bixby-studio input.
upstream_deprecation(bixby_studio_xorg_libs,
    'lost-rob0t/bixby-studio flake.nix lines 128-141 reference xorg.libX*').
upstream_deprecation_emitter(xorg_lib_renames, 'inputs.bixby-studio packages.x86_64-linux.default').
% Note: forcing drvPath alone does NOT trigger the alias warnings;
% forcing outPath does (home-manager build forces outPaths).
warning_probe(attr, outPath).
