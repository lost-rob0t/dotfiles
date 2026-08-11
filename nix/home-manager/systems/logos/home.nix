{ lib, ... }:

{
  imports = [ ../desktop/home.nix ];

  home.username = lib.mkForce "unseen";
  home.homeDirectory = lib.mkForce "/home/unseen";
}
