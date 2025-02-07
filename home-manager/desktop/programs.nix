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
    swiProlog
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
    webkitgtk
    remmina
    freerdp
    sqlitebrowser
    tdesktop
    rpi-imager
    kdenlive
    virt-viewer
    bookworm
    libsForQt5.kdeconnect-kde
    inputs.nixpkgs-stable.legacyPackages.x86_64-linux.ntfy # send notifications

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
  programs = {
    gpg = {
      enable = true;
    };
    emacs = {
      enable = true;
      extraPackages = epkgs: [
        epkgs.vterm
        epkgs.direnv
        epkgs.lsp-pyright
        epkgs.pylint
        epkgs.w3m
        epkgs.pandoc
        pkgs.nodePackages.bash-language-server
        pkgs.roswell
        epkgs.xclip
        pkgs.libnotify # for alert.el
        pkgs.xdotool # for emacs everywhere
        pkgs.ffmpegthumbnailer # Video thumbnails
        pkgs.imagemagick #photo thumbnails
        pkgs.mediainfo #audio previews
        pkgs.mpv # for bongo
        pkgs.pyright
        pkgs.python310
        pkgs.pylint
        pkgs.python310Packages.flake8
        pkgs.mu # Email for mu4e
        pkgs.isync
        pkgs.fetchmail

        epkgs.gnuplot
        pkgs.gnuplot
        pkgs.graphviz
      ];
    };
  };
}
