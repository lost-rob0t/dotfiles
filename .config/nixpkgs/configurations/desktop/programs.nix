{ config, lib, pkgs, ... }:
let
  nyxt = pkgs.nyxt.overrideAttrs (oldAttrs: {
    postFixup = ''
      wrapProgram $out/bin/nyxt \
        --set-default WEBKIT_FORCE_SANDBOX 0
    '';
  });
in
{
  home.packages = [
    pkgs.vlc
    pkgs.obs-studio
    pkgs.libreoffice
    pkgs.filezilla
    pkgs.nerdfonts
    pkgs.discord
    pkgs.cool-retro-term
    pkgs.obs-studio
    pkgs.obsidian
    pkgs.w3m
    pkgs.cht-sh
    pkgs.kleopatra
    pkgs.gimp
    pkgs.feh
    pkgs.mindustry
    pkgs.bookworm
    pkgs.tlp
    nyxt
    pkgs.webkitgtk
    pkgs.remmina
    pkgs.freerdp
    pkgs.sqlitebrowser
    pkgs.tdesktop
    pkgs.rpi-imager
    pkgs.ark
    pkgs.kdenlive
    pkgs.virt-viewer
  ];
  programs = {
      gpg = {
          enable = true;
      };
  };
}
