{ lib, pkgs, config, ... }:
{
  options = with lib; {
    desktop = {
      fonts = {
        enable = mkOption {
          type = types.bool;
          default = true;
        };
        fontsList = mkOption {
          type = types.listOf types.package;
          default = with pkgs; [
            nerd-fonts.hack
            nerd-fonts.symbols-only
            noto-fonts
            noto-fonts-color-emoji
          ];
        };
      };
    };
  };

  config = lib.mkIf config.desktop.fonts.enable {
    home.packages = config.desktop.fonts.fontsList or [ ];
    fonts.fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Hack Nerd Font" ];
        sansSerif = [ "Noto Sans" ];
        serif = [ "Noto Serif" ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };
}
