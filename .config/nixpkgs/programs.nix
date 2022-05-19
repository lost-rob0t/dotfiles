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
    pkgs.obs-studio
    pkgs.obsidian
    pkgs.w3m
    pkgs.cht-sh
    pkgs.kleopatra
    pkgs.insomnia
  ];
}
