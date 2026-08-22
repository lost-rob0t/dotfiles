{ lib, ... }:

{
  imports = [ ../desktop/home.nix ];

  gnome.enable = true;

  home.username = lib.mkForce "unseen";
  home.homeDirectory = lib.mkForce "/home/unseen";
}
