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
    nyxt
    # Development
    gitRepo
    sqlitebrowser
    vim
    direnv
    sbcl
    pre-commit
    swi-prolog
    gnuplot
    ansible
    graphviz
    # Multimedia
    obs-studio
    # BUG kdePackages.kdenlive
    qbittorrent
    picard
    # System Tools
    gparted
    filezilla
    terminator
    remmina
    freerdp
    virt-manager
    virtiofsd

    # Productivity
    recoll
    w3m
    cht-sh
    kdePackages.kleopatra
    gimp
    feh
    mindustry
    activitywatch
    # FIXME nyxt
    remmina
    freerdp
    sqlitebrowser
    telegram-desktop
    virt-viewer
    kdePackages.kdeconnect-kde
    # Security


    # Communication
    #vesktop
    discord
    element-desktop
    thunderbird
    # Misc
	# FIXME Broken package
    #monero-gui
    hugo
    #AI
    ollama
    # GUI Toolkit

    #emojione # wttr widget emojis
    #noto-fonts-emoji
    # TODO make a nixos module for qtile?
    # Can we do qtile without nixos module?
    sxhkd
    conky
    j4-dmenu-desktop
    fetchmail
    variety
    file
    yt-dlp # For Emacs
   inputs.bixby-studio
  ];
}
