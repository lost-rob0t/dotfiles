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
  enabled = evaluate {
    services.starintel = {
      enable = true;
      workDir = root;
      infrastructure.engineReady = true;
      infrastructure.images = lib.genAttrs [ "couchdb" "clouseau" "rabbitmq" "valkey" ] (_: pkgs.hello);
      packages = { server = pkgs.hello; quasar = pkgs.hello; router = pkgs.hello; };
      actors.fixture = { package = pkgs.hello; executable = "hello"; arguments = [ "--greeting=local test" ]; environmentFile = "${root}/config/actor.env"; };
    };
  };
  missingEngine = evaluate { services.starintel.enable = true; };
  units = enabled.config.systemd.user.services;
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
assert lib.all (item: item.assertion) enabled.config.assertions;
assert units.starintel-server.Service.WorkingDirectory == root;
assert lib.elem "starintel-infrastructure.service" units.starintel-server.Unit.Requires;
assert lib.elem "starintel-server.service" units.starintel-quasar.Unit.After;
assert lib.elem "starintel-server.service" units.starintel-actor-fixture.Unit.After;
assert !(builtins.hasAttr "Install" enabled.config.systemd.user.targets.starintel)
  || (enabled.config.systemd.user.targets.starintel.Install.WantedBy or [ ]) == [ ];
pkgs.runCommand "starintel-module-check" {
  nativeBuildInputs = [ pkgs.python3 pkgs.coreutils pkgs.bash ];
  inherit metadata;
  prepare = units.starintel-prepare.Service.ExecStart;
  infrastructure = builtins.head (lib.splitString " " units.starintel-infrastructure.Service.ExecStart);
} ''
  python3 ${./starintel-module.py} "$metadata" "$prepare" "$infrastructure"
  python3 ${./starintel-literate.py} ${../.}
  touch "$out"
''
