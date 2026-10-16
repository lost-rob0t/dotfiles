{ home-manager, pkgs, lib }:
let
  root = "/build/starintel-fixture";
  evaluate = extra: home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      ../nix/home-manager/mods/starintel.nix
      { home.username = "fixture"; home.homeDirectory = "/home/fixture"; home.stateVersion = "24.11"; }
      extra
    ];
  };
  disabled = evaluate { };
  baseConfig = {
    services.starintel = {
      enable = true;
      workDir = root;
      infrastructure.engineReady = true;
      infrastructure.images = lib.genAttrs [ "couchdb" "clouseau" "rabbitmq" "valkey" ] (_: pkgs.hello);
      packages = { server = pkgs.hello; quasar = pkgs.hello; router = pkgs.hello; };
      actors.fixture = { package = pkgs.hello; executable = "hello"; arguments = [ "--greeting=local test" ]; environmentFile = "${root}/config/actor.env"; };
    };
  };
  enabledConfig = lib.recursiveUpdate baseConfig {
    services.starintel.credentials.requireInitialPassword = false;
  };
  enabled = evaluate enabledConfig;
  fresh = evaluate baseConfig;
  manual = evaluate (lib.recursiveUpdate enabledConfig { services.starintel.autoStart = false; });
  invalidPath = evaluate (lib.recursiveUpdate enabledConfig { services.starintel.workDir = "/tmp/bad path"; });
  storeSecret = evaluate (lib.recursiveUpdate enabledConfig { services.starintel.environmentFile = "/nix/store/not-a-runtime-secret"; });
  missingEngine = evaluate { services.starintel.enable = true; };
  units = enabled.config.systemd.user.services;
  credentialPackages = builtins.filter (package: lib.getName package == "starintel-credentials") enabled.config.home.packages;
  credentialCommand = lib.getExe (lib.head credentialPackages);
  metadata = pkgs.writeText "starintel-units.json" (builtins.toJSON {
    inherit root;
    services = units;
    targets = enabled.config.systemd.user.targets;
  });
in
assert !(builtins.hasAttr "starintel" disabled.config.systemd.user.targets);
assert !(builtins.hasAttr "starintel-server" disabled.config.systemd.user.services);
assert disabled.config.services.starintel.workDir == "/home/fixture/.starintel";
assert !(builtins.tryEval missingEngine.config.home.username).success;
assert !(builtins.tryEval invalidPath.config.home.username).success;
assert !(builtins.tryEval storeSecret.config.home.username).success;
assert lib.all (item: item.assertion) enabled.config.assertions;
assert builtins.length credentialPackages == 1;
assert enabled.config.services.starintel.credentials.requireInitialPassword == false;
assert fresh.config.services.starintel.credentials.requireInitialPassword == true;
assert units.starintel-server.Service.WorkingDirectory == root;
assert lib.elem "starintel-infrastructure.service" units.starintel-server.Unit.Requires;
assert lib.elem "starintel-server.service" units.starintel-quasar.Unit.After;
assert lib.elem "starintel-server.service" units.starintel-actor-fixture.Unit.After;
assert enabled.config.systemd.user.targets.starintel.Install.WantedBy == [ "default.target" ];
assert manual.config.systemd.user.targets.starintel.Install.WantedBy == [ ];
assert enabled.config.systemd.user.targets.starintel.Unit.Upholds == enabled.config.systemd.user.targets.starintel.Unit.Wants;
assert lib.elem "starintel-prepare.service" enabled.config.systemd.user.targets.starintel.Unit.Wants;
assert lib.elem "starintel-infrastructure.service" enabled.config.systemd.user.targets.starintel.Unit.Wants;
assert !(builtins.hasAttr "Requires" enabled.config.systemd.user.targets.starintel.Unit);
assert units.starintel-prepare.Unit.Upholds == [ "starintel-infrastructure.service" "starintel-router.service" ];
assert units.starintel-infrastructure.Unit.Upholds == [ "starintel-server.service" ];
assert units.starintel-server.Unit.Upholds == [ "starintel-quasar.service" "starintel-actor-fixture.service" ];
assert units.starintel-prepare.Service.Restart == "on-failure";
assert units.starintel-infrastructure.Service.Restart == "on-failure";
pkgs.runCommand "starintel-module-check" {
  nativeBuildInputs = [ pkgs.python3 pkgs.coreutils pkgs.bash ];
  inherit metadata;
  sbcl = lib.getExe pkgs.sbcl;
  prepare = builtins.head units.starintel-prepare.Service.ExecStart;
  infrastructure = builtins.head (lib.splitString " " (builtins.head units.starintel-infrastructure.Service.ExecStart));
  inherit credentialCommand;
} ''
  python3 ${./starintel-module.py} "$metadata" "$prepare" "$infrastructure" "$credentialCommand"
  python3 ${./starintel-secret-memory.py} ${../nix/home-manager/files/starintel/runtime-secret.py}
  python3 ${./starintel-literate.py} ${../.}
  mkdir "$out"
  cp "$metadata" "$out/units.json"
''
