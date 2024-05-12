{ config, lib, pkgs, ... }:

let

  emacsDired = pkgs.makeDesktopItem {
    name = "dired";
    desktopName = "Dired";
    exec = "emacsclient --eval \"(dired \"%f\")\"";
    terminal = false;
    mimeTypes = [ "application/x-directory" "inode/directory" ];
  };

in

{
  imports = [
    ./services.nix
  ];
  programs.git = {
    enable = true;
    userName = "N545PY";
    userEmail = "nsaspy@fedora.email";
    extraConfig = {
      merge = {
        conflictStyle = "diff3";
      };
    };
  };
  home.packages = with pkgs; [
    scrot
    flameshot
    keepassxc
    nixpkgs-fmt
    xdg-utils
    grc
    # NOTE maybe move to this to desktop
    dunst
    htop
    jq
    gotop # better htop
    fzf # fuzzy finding
    recoll
    rar
    zip
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacs28;
    extraPackages = epkgs: [
      emacsDired # Desktop item
      pkgs.shfmt
      epkgs.khoj
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
      pkgs.python311
      pkgs.python311Packages.flake8
      pkgs.emacsPackages.vterm
      # Pwnagotchi.el
      pkgs.hcxtools
      pkgs.hashcat-utils
      pkgs.hashcat
      # Notifications
      pkgs.libnotify
      pkgs.coreutils
      pkgs.zip
      pkgs.rar
      pkgs.ripgrep
      pkgs.bash # [2023-09-25 Mon] Somehow it was missing this
      pkgs.nodePackages_latest.graphql-language-service-cli
      pkgs.recoll
    ];
  };
  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "application/x-directory" = [ "dired.desktop" ];
      "inode/directory" = [ "dired.desktop" ];
    };
    defaultApplications = {
      "application/x-directory" = [ "dired.desktop" ];
      "inode/directory" = [ "dired.desktop" ];
    };
  };
}
