{ lib, pkgs, config, ... }:

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

    # Keep a user-level Brave launcher independent of session PATH. Home Manager
    # writes this to ~/.local/share/applications/brave-browser.desktop.
    xdg.desktopEntries.brave-browser = {
      name = "Brave Browser";
      genericName = "Web Browser";
      comment = "Browse the web";
      exec = "${pkgs.brave}/bin/brave --new-window %U";
      icon = "brave-browser";
      terminal = false;
      categories = [ "Network" "WebBrowser" ];
      mimeType = [
        "text/html"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
      ];
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
        gotop
        pavucontrol
        xkill
        xdotool
    ];
  };
}
