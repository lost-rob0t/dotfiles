{
  homeConfigurations,
  lib,
  pkgs,
}:

let
  profiles = [
    homeConfigurations."unseen@desktop"
    homeConfigurations."unseen@flake"
    homeConfigurations."unseen@hunter02"
  ];
  runtimeUnits = [
    "qwen3-tts"
    "zara-desktop"
    "zara-pair"
    "zara-server"
    "zara-wake"
  ];
  hasRuntimeUnit = profile: unit: builtins.hasAttr unit profile.config.systemd.user.services;
  hasAnyRuntimeUnit = profile: lib.any (hasRuntimeUnit profile) runtimeUnits;
in
assert lib.all (profile: !profile.config.zara.enable) profiles;
assert lib.all (profile: !profile.config.zara.workflows.enable) profiles;
assert lib.all (profile: !hasAnyRuntimeUnit profile) profiles;
assert !homeConfigurations."unseen@desktop".config.services.qwen3-tts.enable;
pkgs.runCommand "zara-runtime-disabled-check" { } ''
  touch "$out"
''
