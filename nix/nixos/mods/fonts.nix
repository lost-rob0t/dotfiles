{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktopEnv = {
      fonts = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Enable the fonts and configure them. By default nerd fonts are used.";
        };
        fontsList = mkOption {
          type = types.listOf types.package;
          default = with pkgs; [ nerd-fonts.hack nerd-fonts.symbols-only ];
        };
      };
    };
  };

  config = lib.mkIf config.desktopEnv.fonts.enable {
    environment.systemPackages = (config.desktopEnv.fonts.fontsList or [  ]);
    fonts.fontconfig.enable = true;
  };
}
