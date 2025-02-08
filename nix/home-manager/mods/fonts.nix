{ lib, pkgs, config, ... }: {
  options = with lib; {
    rice = {
      fonts = {
        enable = mkOption {
          type = types.bool;
          default = true;
        };
        extraFonts = mkOption {
          type = types.listOf types.package;
          default = with pkgs; [ nerd-fonts.hack nerd-fonts.symbols-only];
        };
      };
    };
  };

  config = lib.mkIf config.rice.fonts.enable {
    home.packages = config.rice.fonts.extraFonts;
    fonts.fontconfig.enable = true;
  };
}
