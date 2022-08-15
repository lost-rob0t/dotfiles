{ config, lib, pkgs, ... }:
let
  nyxt = pkgs.nyxt.overrideAttrs (oldAttrs: {
    postFixup = ''
      wrapProgram $out/bin/nyxt \
        --set-default WEBKIT_FORCE_SANDBOX 0
    '';
  });
  name = "N545PY";
  email = "nsaspy@airmail.cc";

in
{
  home.packages = [
    pkgs.vlc
    pkgs.obs-studio
    pkgs.libreoffice
    pkgs.filezilla
    pkgs.nerdfonts
    pkgs.w3m
    pkgs.cht-sh
    pkgs.kleopatra
    pkgs.gimp
    pkgs.feh
    pkgs.bookworm
    pkgs.tlp
    nyxt
    pkgs.webkitgtk
    pkgs.remmina
    pkgs.freerdp
    pkgs.sqlitebrowser
    pkgs.brave
  ];
  programs = {
      git = {
        enable = true;
        userName = "${name}";
        userEmail = "${email}";
      };
  emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
};
  };


}
