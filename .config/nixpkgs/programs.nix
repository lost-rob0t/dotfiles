{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.vlc
    pkgs.obs-studio
    pkgs.libreoffice
    pkgs.lutris
    pkgs.filezilla
  ];
}
