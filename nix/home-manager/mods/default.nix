{ config, pkgs, ... }:
{
  imports = [
    ./base.nix
    ./desktop.nix
    ./fonts.nix
    ./emacs.nix
    ./security.nix
    #./pentesting.nix
    #./font.nix
  ];
}
