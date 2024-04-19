{ inputs, config, lib, pkgs, ... }:

{
  imports = ["flake.nix"];
  system.nixos.tags = ["kde"];
  services = {
    xorg = {
      desktopManager = {
        plasma5 = {
          enable = true;
        };
      };
    };
  };


}
