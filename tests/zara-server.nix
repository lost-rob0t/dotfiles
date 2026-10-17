{
  homeConfigurations,
  lib,
  pkgs,
}:

let
  flakeHome = homeConfigurations."unseen@flake";
  profiles = [
    homeConfigurations."unseen@desktop"
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
assert flakeHome.config.zara.enable;
assert flakeHome.config.zara.workflows.enable;
assert flakeHome.config.zara.server.enable;
assert flakeHome.config.zara.desktop.enable;
assert flakeHome.config.zara.wake.enable;
assert flakeHome.config.services.qwen3-tts.enable;
assert flakeHome.config.services.qwen3-tts.backend == "cpu";
assert hasRuntimeUnit flakeHome "zara-server";
assert hasRuntimeUnit flakeHome "zara-wake";
assert hasRuntimeUnit flakeHome "qwen3-tts";
pkgs.runCommand "zara-profile-check" { } ''
  touch "$out"
''
