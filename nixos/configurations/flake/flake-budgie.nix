{ inputs, config, lib, pkgs, ... }:

{
  imports = ["flake.nix"];
  system.nixos.tags = ["budgie"];
  services = {
    xorg = {
      desktopManager = {
        budgie = {
          enable = true;
        };
      };
    };
  };


}
