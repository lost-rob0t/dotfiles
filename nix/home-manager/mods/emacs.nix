{ lib, pkgs, config, ... }: {

  options = with lib; {
    emacs = {
      enable = mkOption {
      type = types.bool;
      description = "Enable emacs";
      default = true;
    };
      package = mkOption {
        type =
      };
    gitUser = mkOption {
      type = types.string;
      default = home.user;
      description = "Configure Git to use this username";
    };
    gitEmail = mkOption {
      type = types.string;
      description = "Configure Git to use this email.";
    };
    diredXDG = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Create XDG associations for file management via dired.";
      };

      pkg = mkOption {
        type = types.package;
        description = "Desktop/XDG association for files in emacs.";
      };
    };
    extraPackages = mkOption {
        type = types.listOf types.package;
        default = [];
        description = "Extra packages to install ontop of the base ones included with this module";
      };
  };
  };
  config = with lib; mkIf config.emacs.enable {
    emacs.diredXDG.pkg = pkgs.makeDesktopItem {
      name = "dired";
      desktopName = "Dired";
      exec = "emacsclient --eval \"(dired \"%f\")\"";
      terminal = false;
      mimeTypes = [ "application/x-directory" "inode/directory" ];
    };
    programs.git = {
      enable = true;
      userName = "N545PY"; # If you use my modules change this
      userEmail = "nsaspy@fedora.email";
      extraConfig = {
        merge = {
        conflictStyle = "diff3";
        };
      };
    };
    programs.gpg.enable = true;
    programs.emacs = {
      enable = true;
      package = config.emacs.package;
      extraPackages = epkgs: ([
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
        # TODO we need a single source of truth for python versions!
        pkgs.python311
        pkgs.python311Packages.flake8
        # Notifications
        pkgs.libnotify
        pkgs.coreutils
        pkgs.zip
        pkgs.rar
        pkgs.ripgrep
        pkgs.bash
        pkgs.nodePackages_latest.graphql-language-service-cli
        pkgs.recoll
        pkgs.aspell
      (pkgs.aspellWithDicts
        (dicts: with dicts; [ en en-computers en-science  ]))

        pkgs.yt-dlp

        ## ensure emacs gets the fonts!
      ] ++ (config.desktop.fonts.fontsList or [])
        ++ (config.emacs.extraPackages or [])
      );};

    xdg = mkIf config.emacs.diredXDG.enable {
      mimeApps = {
        enable = true;
        associations.added = {
          "application/x-directory" = [ "$config.emacs.diredXDG.pkg" ];
          "inode/directory" = [ "$config.emacs.diredXDG.pkg" ];
        };
    defaultApplications = {
      "application/x-directory" = [ "$config.emacs.diredXDG.pkg" ];
      "inode/directory" = [ "$config.emacs.diredXDG.pkg" ];
    };
  };
    };
  };
}
