{ config, lib, pkgs, inputs, ... }:

# this really doesnt work in nyxt yet
#let
#  nyxt = pkgs.nyxt.overrideAttrs (oldAttrs: {
#    postFixup = ''
#      wrapProgram $out/bin/nyxt \
#        --set-default WEBKIT_FORCE_SANDBOX 0
#    '';
#  });
#  in
#
{

  home.packages = with pkgs; [
    # Development
    gitRepo
    sqlitebrowser
    vim
    direnv
    pre-commit
    gnuplot
    ansible
    graphviz
    # Multimedia
    # System Tools
    gparted
    filezilla
    terminator
    remmina
    freerdp

    # Productivity
    w3m
    cht-sh
    # TODO Add some of this stuff to a modulekleopatra
    gimp
    feh
    nyxt
    remmina
    freerdp
    sqlitebrowser
    telegram-desktop
    # Misc
    sxhkd
    conky
    j4-dmenu-desktop
    fetchmail
    variety
    file
  ];
}
