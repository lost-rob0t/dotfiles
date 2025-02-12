{ inputs, config, lib, pkgs, ... }:

{
  system.nixos.tags = ["budgie"];
  services = {
    xserver = {
      desktopManager = {
        budgie = {
          enable = true;
        };
      };
    };
  };


}
