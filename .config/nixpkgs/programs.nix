{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.vlc
    pkgs.obs-studio
    pkgs.libreoffice
    pkgs.lutris
    pkgs.filezilla
    pkgs.nerdfonts
    pkgs.discord
    pkgs.cool-retro-term
  ];
}
