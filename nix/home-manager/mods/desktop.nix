{ lib, pkgs, config, ... }:

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
        kdePackages.spectacle
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
