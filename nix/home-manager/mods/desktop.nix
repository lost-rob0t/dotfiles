{ lib, pkgs, config, ... }:

{
  options = {
    # Enable desktop configuration.
    desktop.enable = lib.mkEnableOption "Automatically configure the desktop";
    # Define a list option for the base desktop packages.
    hibernateDesktop = lib.mkOption {
      type = lib.types.package;
    };
  };

  config = lib.mkIf config.desktop.enable {
    # Define a desktop item (for example, a Hibernate shortcut)
    hibernateDesktop = pkgs.makeDesktopItem {
      name = "Hibernate";
      desktopName = "Hibernate";
      exec = "${pkgs.pmutils}/bin/pm-hibernate";
      terminal = false;
    };

    rice.fonts.enable = true;
    home.packages = with pkgs;  [
        brave
        xsettingsd
        firefox
        scrot
        flameshot
        keepassxc
        xdg-utils
        dunst
        gotop
        recoll
        nim
        nimble];
  };
}
