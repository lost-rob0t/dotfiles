{
  homeConfigurations,
  lib,
  pkgs,
  quasar,
  star-router,
  starintel-server,
}:
let
  system = pkgs.stdenv.hostPlatform.system;
  host = homeConfigurations."unseen@flake";
  cfg = host.config.services.starintel;
  serverPackages = starintel-server.packages.${system};
in
assert cfg.enable;
assert cfg.autoStart;
assert cfg.workDir == "/home/unseen/.starintel";
assert !cfg.credentials.requireInitialPassword;
assert cfg.actors == { };
assert cfg.packages.server == serverPackages.default;
assert cfg.packages.quasar == quasar.packages.${system}.quasar-server;
assert cfg.packages.router == star-router.packages.${system}.default;
assert cfg.infrastructure.engineReady;
assert cfg.infrastructure.couchdbUser == "admin";
assert
  cfg.infrastructure.images == {
    couchdb = serverPackages.couchdb-image;
    clouseau = serverPackages.clouseau-image;
    rabbitmq = serverPackages.rabbitmq-image;
    valkey = serverPackages.valkey-image;
  };
assert lib.all (item: item.assertion) host.config.assertions;
pkgs.runCommand "starintel-host-check" { } ''
  touch "$out"
''
