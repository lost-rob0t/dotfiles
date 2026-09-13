% nixpkgs deprecation mappings enforced by tests/nix-deprecations.sh.

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
%   nix/home-manager/mods/games.nix (mousetrap defaultPackage)
%   nix/nixos/systems/flake/packages.nix (xorg.xinit)

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
