{ config, lib, pkgs, ... }:

#  this really doesnt work in nyxt yet
#let
#  nyxt = pkgs.nyxt.overrideAttrs (oldAttrs: {
#    postFixup = ''
#      wrapProgram $out/bin/nyxt \
#        --set-default WEBKIT_FORCE_SANDBOX 0
#    '';
#  });
#  in
{

  home.packages = with pkgs; [
    xdotool
    xorg.xwininfo
    # TODO sort into categories
    vlc
    obs-studio
    gparted
    filezilla
    terminator
    discord
    obs-studio
    obsidian
    w3m
    cht-sh
    kleopatra
    gimp
    feh
    mindustry
    tlp
    nyxt
    webkitgtk
    remmina
    freerdp
    sqlitebrowser
    tdesktop
    rpi-imager
    ark
    kdenlive
    virt-viewer
    monero-gui
    gitRepo
    vscodium-fhs # FHS
    librewolf
    bookworm
    zeal
    brave
    thunderbird
    vim
    virt-manager
    virtiofsd
    element-desktop
    qbittorrent
    terminator
    lbry ## You should use lbry and other yotube alternatives
    yt-dlp
    libsForQt5.kdeconnect-kde
    ntfy # send notifications
    mpv
    # Security
    keepassxc
    hcxtools
    hashcat-utils
    # rice
    starship
    j4-dmenu-desktop
    emojione # wttr widget emojis
    noto-fonts-emoji
    grc # colourize output

    sonixd # self hosted music streaming
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
        pkgs.aspell
        pkgs.aspellDicts.en
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

      ];
    };
  };
}
