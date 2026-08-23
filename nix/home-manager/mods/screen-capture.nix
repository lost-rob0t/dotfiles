{ config, lib, pkgs, ... }:

let
  cfg = config.screenCapture;

  screenCapturePackage = pkgs.writeShellApplication {
    name = "screen-capture";
    runtimeInputs = with pkgs; [
      ffmpeg
      grim
      libnotify
      maim
      slop
      slurp
      wf-recorder
      wl-clipboard
      xclip
      xdotool
      xorg.xrandr
    ];
    text = builtins.readFile ../../../.local/bin/screen-capture;
  };
in
{
  options.screenCapture = {
    enable = lib.mkEnableOption "reliable screenshots and screen recordings";

    screenshotDirectory = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Pictures/screen-shots";
      description = "Directory used for screenshots.";
    };

    recordingDirectory = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Videos";
      description = "Default directory used for screen recordings.";
    };

    fps = lib.mkOption {
      type = lib.types.ints.positive;
      default = 60;
      description = "Recording frame rate.";
    };

    copyScreenshots = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Copy screenshot image data to the clipboard after capture.";
    };

    copyRecordingPath = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Copy the completed recording path to the clipboard.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ screenCapturePackage ];

    home.sessionVariables = {
      SCREEN_CAPTURE_SCREENSHOT_DIR = cfg.screenshotDirectory;
      SCREEN_CAPTURE_VIDEO_DIR = cfg.recordingDirectory;
      SCREEN_CAPTURE_FPS = toString cfg.fps;
      SCREEN_CAPTURE_COPY_SCREENSHOTS = if cfg.copyScreenshots then "1" else "0";
      SCREEN_CAPTURE_COPY_RECORDING_PATH = if cfg.copyRecordingPath then "1" else "0";
    };

    home.activation.screenCaptureDirectories = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir -p \
        ${lib.escapeShellArg cfg.screenshotDirectory} \
        ${lib.escapeShellArg cfg.recordingDirectory}
    '';
  };
}
