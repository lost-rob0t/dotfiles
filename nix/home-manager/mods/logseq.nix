{ config, lib, pkgs, ... }:

let
  cfg = config.desktop.logseq;

  notesBridge = pkgs.writeShellApplication {
    name = "notes-bridge";
    runtimeInputs = [ pkgs.python3 ];
    text = ''
      exec ${pkgs.python3}/bin/python3 ${../../../scripts/notes-bridge.py} "$@"
    '';
  };
in
{
  options.desktop.logseq = {
    enable = lib.mkEnableOption "Logseq and the Logseq <-> Org-roam notes bridge";

    logseqDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Notes/log";
      description = "Path to the file-based Logseq graph.";
    };

    roamDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Notes/org/roam";
      description = "Path to the Org-roam note directory.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.logseq
      notesBridge
    ];

    home.sessionVariables = {
      NOTES_LOGSEQ_DIR = cfg.logseqDir;
      NOTES_ORG_ROAM_DIR = cfg.roamDir;
    };
  };
}
