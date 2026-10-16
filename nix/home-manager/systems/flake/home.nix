{ inputs, pkgs, ... }:
let
  system = pkgs.stdenv.hostPlatform.system;
  serverPackages = inputs.starintel-server.packages.${system};
in
{
  imports = [ ../desktop/home.nix ];

  node-red.enable = true;

  services.starintel = {
    enable = true;
    workDir = "/home/unseen/.starintel";
    credentials.requireInitialPassword = false;
    packages = {
      server = serverPackages.default;
      quasar = inputs.quasar.packages.${system}.quasar-server;
      router = inputs.star-router.packages.${system}.default;
    };
    infrastructure = {
      engineReady = true;
      couchdbUser = "admin";
      images = {
        couchdb = serverPackages.couchdb-image;
        clouseau = serverPackages.clouseau-image;
        rabbitmq = serverPackages.rabbitmq-image;
        valkey = serverPackages.valkey-image;
      };
    };
  };

  homeManagerUpdater = {
    enable = true;
    hostName = "flake";
  };

  # Zara is temporarily disabled by the shared desktop profile. Keep the
  # host-specific listener dormant until the runtime is deliberately restored.
  # zara.server = {
  #   remoteEndpoint = "tcp://0.0.0.0:6060";
  #   securityDir = "/home/unseen/.local/state/zarathushtra/security";
  # };

  nixGl.enable = true;
}
