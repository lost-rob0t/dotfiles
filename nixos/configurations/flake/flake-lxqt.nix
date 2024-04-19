{ inputs, config, lib, pkgs, ... }:

{
  imports = ["flake.nix"];
  system.nixos.tags = ["lxqt"];
  services = {
    xorg = {
      desktopManager = {
        lxqt = {
          enable = true;
        };
      };
    };
  };


}
