{
  lib,
  pkgs,
  config,
  inputs,
  ...
}:
let
  nativePackageManifest =
    builtins.fromJSON (builtins.readFile ../../../emacs/desktop/packages.json);

  nativePackageAliases = {
    emacs-async = "async";
    ppcre2el = "pcre2el";
    "pcap-mode.el" = "pcap-mode";
    "podman.el" = "podman";
  };

  nativeBuiltinPackages = [
    "css-mode"
    "elisp-mode"
    "flymake"
    "hideshow"
    "rx"
    "smerge-mode"
    "vc"
    "vc-annotate"
  ];

  nativePackageName = name: nativePackageAliases.${name} or name;

  nativeEmacsPackages =
    epkgs:
    let
      names = lib.unique (map nativePackageName nativePackageManifest.packages);
      missing = builtins.filter (
        name:
        !(builtins.elem name nativeBuiltinPackages)
        && !(builtins.hasAttr name epkgs)
      ) names;
      available = builtins.filter (
        name:
        !(builtins.elem name nativeBuiltinPackages)
        && builtins.hasAttr name epkgs
      ) names;
    in
    lib.warnIf (missing != [ ])
      "Native Emacs manifest has unresolved packages: ${lib.concatStringsSep ", " missing}"
      (map (name: builtins.getAttr name epkgs) available);
in
{
  imports = [
    ./gpt-todos.nix
  ];

  options = with lib; {
    emacs = {
      enable = mkOption {
        type = types.bool;
        description = "Enable emacs";
        default = true;
      };
      package = mkOption {
        type = types.package;
        default = pkgs.emacs;
        description = "Which emacs package to use?";
      };
      gitUser = mkOption {
        type = types.str;
        default = config.home.username;
        description = "Configure Git to use this username";
      };
      gitEmail = mkOption {
        type = types.str;
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
        default = [ ];
        description = "Extra packages to install on top of the native desktop closure";
      };
    };
  };

  config = with lib; mkIf config.emacs.enable {
    home.sessionVariables = {
      STARINTEL_SOCIAL_ROOT = "${config.home.homeDirectory}/starintel/starintel-social-presence";
    };

    emacs.diredXDG.pkg = pkgs.makeDesktopItem {
      name = "dired";
      desktopName = "Dired";
      exec = "emacsclient --eval \"(dired \\\"%f\\\")\"";
      terminal = false;
      mimeTypes = [
        "application/x-directory"
        "inode/directory"
      ];
    };

    programs.git = {
      enable = true;
      settings = {
        user = {
          name = config.emacs.gitUser;
          email = config.emacs.gitEmail;
        };
        merge.conflictStyle = "diff3";
      };
    };

    programs.emacs = {
      enable = true;
      package = config.emacs.package;
      extraPackages =
        epkgs:
        [
          (inputs.emacs-auto-research.lib.mkPackage { inherit pkgs epkgs; })
          pkgs.shfmt
          epkgs.khoj
          epkgs.vterm
          epkgs.direnv
          epkgs.lsp-pyright
          epkgs.pylint
          epkgs.w3m
          epkgs.pandoc
          epkgs.xclip
          pkgs.aspell
          pkgs.aspellDicts.en
          pkgs.libnotify
          pkgs.xdotool
          pkgs.ffmpegthumbnailer
          pkgs.imagemagick
          pkgs.mediainfo
          pkgs.mpv
          pkgs.pyright
          pkgs.python311
          pkgs.python311Packages.flake8
          pkgs.libnotify
          pkgs.coreutils
          pkgs.zip
          pkgs.rar
          pkgs.ripgrep
          pkgs.bashInteractive
          pkgs.recoll
          pkgs.xwininfo
          pkgs.xdotool
          pkgs.fd
        ]
        ++ nativeEmacsPackages epkgs
        ++ config.emacs.extraPackages;
    };

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
