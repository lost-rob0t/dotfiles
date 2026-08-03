{ lib, ... }:

{
  imports = [ ../desktop/home.nix ];

  home.username = lib.mkForce "useen";
  home.homeDirectory = lib.mkForce "/home/useen";
}
