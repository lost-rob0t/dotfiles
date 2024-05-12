{ inputs, config, lib, pkgs, ... }:

{
  system.nixos.tags = ["lxqt"];
  services = {
    xserver = {
      desktopManager = {
        lxqt = {
          enable = true;
        };
      };
    };
  };


}
