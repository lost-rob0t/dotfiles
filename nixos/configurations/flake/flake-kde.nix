{ inputs, config, lib, pkgs, ... }:

{
  system.nixos.tags = ["kde"];
  services = {
    xserver = {
      desktopManager = {
        plasma5 = {
          enable = true;
        };
      };
    };
  };


  xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-kde;];
  };
}
