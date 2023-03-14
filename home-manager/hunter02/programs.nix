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
    pkgs.filezilla
    #pkgs.nerdfonts
    pkgs.w3m
    pkgs.cht-sh
    pkgs.kleopatra
    pkgs.gimp
    pkgs.feh
    pkgs.tlp
    nyxt
    pkgs.webkitgtk
    pkgs.remmina
    pkgs.freerdp
    pkgs.sqlitebrowser
    pkgs.ark
    pkgs.direnv
    pkgs.starship
    pkgs.rlwrap
    pkgs.tor-browser-bundle-bin
    pkgs.brave
    pkgs.cloc
  ];
  programs = {
      gpg = {
          enable = true;
      };
  };
}
