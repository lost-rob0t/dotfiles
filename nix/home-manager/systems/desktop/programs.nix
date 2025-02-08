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
    sbcl
    erlang
    pre-commit
    swi-prolog
    gnuplot
    ansible
    graphviz
    # Multimedia
    vlc
    simplescreenrecorder
    obs-studio
    gimp
    feh
    kdenlive
    mpv
    qbittorrent
    yt-dlp
    sonixd # self hosted music streaming
    picard
    # Games
    inputs.mousetrap.defaultPackage.x86_64-linux
    # System Tools
    gparted
    filezilla
    terminator
    remmina
    freerdp
    rpi-imager
    ark
    virt-manager
    virtiofsd

    # Productivity
    w3m
    cht-sh
    kleopatra
    gimp
    feh
    mindustry
    activitywatch
    # FIXME nyxt
    remmina
    freerdp
    sqlitebrowser
    tdesktop
    rpi-imager
    kdenlive
    virt-viewer
    bookworm
    libsForQt5.kdeconnect-kde
    ntfy

    # Security
    hcxtools
    hashcat-utils
    hashcat

    # Web Browsers
    brave
    firefox
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
    webkitgtk

    # rice
    starship
    #emojione # wttr widget emojis
    #noto-fonts-emoji
    grc # colourize output
    sxhkd
    conky
    j4-dmenu-desktop
    fetchmail
    variety
    pavucontrol
    file


  ];
}
