{ lib, pkgs, ... }:
let
  mpvScripts = pkgs.mpvScripts; # Assuming mpvScripts is provided by pkgs
in {
  options.desktop.media.mpv.pkg = lib.mkOption {
    type = lib.types.package;
    default = pkgs.mpv.override {
      scripts = [
        mpvScripts.thumbnail
        mpvScripts.sponsorblock
        mpvScripts.mpv-notify-send
        mpvScripts.videoclip
        mpvScripts.mpv-webm
        mpvScripts.memo
        mpvScripts.modernx
        mpvScripts.autocrop
        mpvScripts.quality-menu
        mpvScripts.mpris
      ];
    };
    description = "Customized MPV package with additional scripts.";
  };
}
