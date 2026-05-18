{ lib, pkgs, config, ... }: {
  options = with lib; {
    desktop.media = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable default media programs.";
      };
      mpv = {
        pkg = mkOption {
          type = types.package;
          description = "Customized MPV package with extra scripts.";
        };
      };
    };
  };

  config = with lib; mkIf config.desktop.media.enable {
    desktop.media.mpv.pkg = pkgs.mpv.override {
      scripts = with pkgs.mpvScripts; [
        thumbnail
        sponsorblock
        mpv-notify-send
        videoclip
        mpv-webm
        memo
        modernx
        autocrop
        quality-menu
        mpris
      ];
    };

    home.packages = with pkgs; [
      vlc
      config.desktop.media.mpv.pkg
      simplescreenrecorder
      feh
      gimp
      sonixd # Self-hosted music streaming
    ];
  };
}
