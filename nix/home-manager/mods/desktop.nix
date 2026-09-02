{ lib, pkgs, config, ... }:

let
  qtileWallpaperScript = pkgs.writeText "variety-set-qtile.sh" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    wallpaper="''${1:-}"
    if [[ -z "$wallpaper" || ! -f "$wallpaper" ]]; then
      printf 'set_qtile.sh: invalid wallpaper: %s\n' "$wallpaper" >&2
      exit 2
    fi

    exec ${pkgs.feh}/bin/feh --bg-fill "$wallpaper"
  '';
  varietyQtileScript = "${config.xdg.configHome}/variety/scripts/set_qtile.sh";
in
{
  options = {
    desktop =  {
    # Enable desktop configuration.
      enable = lib.mkEnableOption "Automatically configure the desktop";
    # Define a list option for the base desktop packages.
      hibernateDesktop = lib.mkOption {
        type = lib.types.package;
      };
      pavucontrolDesktop = lib.mkOption {
        type = lib.types.package;
      };
    };
  };

  config = lib.mkIf config.desktop.enable {
    # LightDM sources ~/.xprofile before starting Qtile. Keep that file under
    # Home Manager ownership so a fresh checkout/activation cannot silently
    # omit the session environment merely because `stow .` was not re-run.
    home.file.".xprofile" = {
      source = ../../../.xprofile;
      force = true;
    };

    # Variety chmods scripts in its live config directory during startup, so
    # this path cannot be a Home Manager symlink into the immutable Nix store.
    # Keep the source declarative, but install a fresh writable runtime copy on
    # every activation. The rm also migrates the old store-backed symlink.
    home.activation.varietyQtileScript = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      target=${lib.escapeShellArg varietyQtileScript}
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm -f "$target"
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/install -Dm700 \
        ${qtileWallpaperScript} \
        "$target"
    '';

    # Put Brave in XDG_DATA_HOME as well as the Nix profile. j4-dmenu-desktop
    # always searches the user application directory, so discovery no longer
    # depends solely on XDG_DATA_DIRS inherited by the current X session.
    xdg.desktopEntries.brave = {
      name = "Brave";
      genericName = "Web Browser";
      exec = "${pkgs.brave}/bin/brave %U";
      icon = "brave-browser";
      terminal = false;
      categories = [ "Network" "WebBrowser" ];
    };

    # Define a desktop item (for example, a Hibernate shortcut)
    desktop.hibernateDesktop = pkgs.makeDesktopItem {
      name = "Hibernate";
      desktopName = "Hibernate";
      exec = "${pkgs.pmutils}/bin/pm-hibernate";
      terminal = false;
    };
    desktop.pavucontrolDesktop = pkgs.makeDesktopItem {
      name = "Hibernate";
      desktopName = "Hibernate";
      exec = "${pkgs.pavucontrol}/bin/pavucontrol";
      terminal = false;
    };


    home.packages = with pkgs;  [
        brave
        firefox
        xsettingsd
        scrot
        keepassxc
        xdg-utils
        dunst
        dmenu
        gotop
        pavucontrol
        xkill
        xdotool
    ];
  };
}
