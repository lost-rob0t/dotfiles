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
        flameshot
        keepassxc
        xdg-utils
        dunst
        gotop
        pavucontrol
    ];
  };
}
