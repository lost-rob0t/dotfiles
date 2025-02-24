{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktop = {
      fonts = {
        enable = mkOption {
          type = types.bool;
          default = mkDefault true;
          description = "Enable the fonts and configure them. By default nerd fonts are used.";
        };
        fontsList = mkOption {
          type = types.listOf types.package;
          default = with pkgs; [
      nerd-fonts.hack
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      liberation_ttf
      fira-code
      fira-code-symbols
      mplus-outline-fonts.githubRelease
      dina-font
      proggyfonts
 ];
        };
      };
    };
  };

  config = lib.mkIf config.desktop.fonts.enable {
    environment.systemPackages = (config.desktop.fonts.fontsList or [  ]);
    fonts.fontconfig.enable = true;
  };
}
