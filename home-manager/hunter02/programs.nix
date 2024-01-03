{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    sonixd
    vlc
    filezilla
    #nerdfonts
    w3m
    cht-sh
    kleopatra
    gimp
    feh
    tlp
    nyxt
    webkitgtk
    remmina
    freerdp
    sqlitebrowser
    ark
    direnv
    starship
    rlwrap
    tor-browser-bundle-bin
    brave
    cloc

    sbcl

    #hackerman
    webanalyze
    wafw00f
    go-dork
    nikto
    cloudhunter
    whatweb
    wpscan
    naabu

    nodePackages.uglify-js
    # Graphql related stuff
    altair
    inql
    graphw00f
  ];
  programs = {
    gpg = {
      enable = true;
    };
  };
}
