# Invoked by starintel-live.py with a newly-created, disposable /tmp root.
# This evaluates the real module without evaluating a desktop activation package.
{ root, system ? builtins.currentSystem }:
let
  repository = builtins.getFlake (toString ../.);
  pkgs = repository.inputs.nixpkgs.legacyPackages.${system};
  lib = pkgs.lib;
  sources = {
    server = "github:lost-rob0t/starintel-server/491b89dad31e15b4172d52a03ab525b6c92fd637";
    quasar = "github:lost-rob0t/quasar/9079604f766990db7f9b8998705862c4fa914c45";
    router = "github:lost-rob0t/starRouter/0a4e9abd073924e9186482fe847034b4fc4c401b";
  };
  upstream = lib.mapAttrs (_: ref: (builtins.getFlake ref).packages.${system}) sources;
  packages = {
    server = upstream.server.default;
    quasar = upstream.quasar.quasar-server;
    router = upstream.router.default;
  };
  images = lib.genAttrs [ "couchdb" "clouseau" "rabbitmq" "valkey" ]
    (name: upstream.server."${name}-image");
  evaluated = repository.inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      ../nix/home-manager/mods/starintel.nix
      {
        home.username = "starintel-ci";
        home.homeDirectory = "${root}/home";
        home.stateVersion = "24.11";
        services.starintel = {
          enable = true;
          workDir = root;
          inherit packages;
          infrastructure = { inherit images; engineReady = true; };
          # No actors, credentials, developer bypass, or production state.
          actors = { };
        };
      }
    ];
  };
  names = [ "starintel-prepare" "starintel-infrastructure" "starintel-server"
    "starintel-quasar" "starintel-router" ];
  manifest = pkgs.writeText "starintel-live-manifest.json" (builtins.toJSON {
    inherit root system sources;
    packages = lib.mapAttrs (_: value: toString value) packages;
    images = lib.mapAttrs (_: value: toString value) images;
    services = lib.genAttrs names (name: evaluated.config.systemd.user.services.${name}.Service);
    docker = "${pkgs.docker-client}/bin/docker";
    compose = "${pkgs.docker-compose}/bin/docker-compose";
  });
in
assert system == "x86_64-linux";
assert builtins.match "/tmp/starintel-live-[a-zA-Z0-9_-]+" root != null;
assert lib.all (item: item.assertion) evaluated.config.assertions;
assert evaluated.config.services.starintel.actors == { };
pkgs.linkFarm "starintel-live-fixture" (
  [ { name = "manifest.json"; path = manifest; } ]
  ++ lib.mapAttrsToList (name: path: { name = "packages/${name}"; inherit path; }) packages
  ++ lib.mapAttrsToList (name: path: { name = "images/${name}"; inherit path; }) images
)
