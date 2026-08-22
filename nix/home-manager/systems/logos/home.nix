{ lib, ... }:

{
  imports = [ ../desktop/home.nix ];

  desktop.gnomeTablet.enable = true;

  home.username = lib.mkForce "unseen";
  home.homeDirectory = lib.mkForce "/home/unseen";
}
