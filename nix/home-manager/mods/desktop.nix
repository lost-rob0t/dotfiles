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

    # Keep Brave visible to launchers even when the session misses Home Manager's
    # XDG_DATA_DIRS. Home Manager owns the literal user-level desktop entry.
    home.file.".local/share/applications/brave-browser.desktop".text = ''
      [Desktop Entry]
      Version=1.0
      Type=Application
      Name=Brave Browser
      GenericName=Web Browser
      Comment=Browse the web
      Exec=${pkgs.brave}/bin/brave --new-window %U
      Icon=brave-browser
      Terminal=false
      Categories=Network;WebBrowser;
      MimeType=text/html;x-scheme-handler/http;x-scheme-handler/https;
    '';


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
