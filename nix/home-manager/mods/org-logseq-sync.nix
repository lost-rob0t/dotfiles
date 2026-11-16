{ config, lib, pkgs, ... }:

let
  cfg = config.orgLogseqSync;

  orgLogseqSync = pkgs.writeShellApplication {
    name = "org-logseq-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.inotify-tools
      pkgs.unison
      pkgs.util-linux
      config.emacs.package
    ];
    text = builtins.readFile ../../../scripts/org-logseq-sync;
  };
in
{
  options.orgLogseqSync = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Keep Org/org-roam notes synchronized bidirectionally with a Logseq graph.";
    };

    orgRoamDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Notes/org/roam";
      description = "Org-roam root containing normal notes and the dailies subtree.";
    };

    orgRoamDailiesDir = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "Path below orgRoamDir mapped to Logseq journals.";
    };

    logseqGraphDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Notes/logseq";
      description = "Logseq graph root whose pages and journals directories are synchronized.";
    };

    debounceSeconds = lib.mkOption {
      type = lib.types.str;
      default = "1";
      description = "Delay after a filesystem event before reconciliation.";
    };

    refreshOrgRoamDb = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Ask a running Emacs server to refresh the org-roam database after a clean sync.";
    };

    emacsServerName = lib.mkOption {
      type = lib.types.str;
      default = "doom";
      description = "Emacs server socket name used for org-roam database refreshes.";
    };
  };

  config = lib.mkIf (config.emacs.enable && cfg.enable) {
    home.packages = [ orgLogseqSync ];

    home.sessionVariables = {
      ORG_ROAM_DIR = cfg.orgRoamDir;
      ORG_ROAM_DAILIES_DIR = cfg.orgRoamDailiesDir;
      LOGSEQ_GRAPH_DIR = cfg.logseqGraphDir;
      ORG_LOGSEQ_SYNC_DEBOUNCE_SECONDS = cfg.debounceSeconds;
      ORG_LOGSEQ_SYNC_REFRESH_ROAM = if cfg.refreshOrgRoamDb then "1" else "0";
      ORG_LOGSEQ_SYNC_EMACS_SERVER_NAME = cfg.emacsServerName;
    };

    systemd.user.services.org-logseq-sync = {
      Unit = {
        Description = "Bidirectional Org-roam and Logseq synchronization";
        After = [ "default.target" ];
      };

      Service = {
        Type = "simple";
        ExecStart = "${orgLogseqSync}/bin/org-logseq-sync --watch";
        Restart = "on-failure";
        RestartSec = "2s";
        UMask = "0077";
        Environment = [
          "ORG_ROAM_DIR=${cfg.orgRoamDir}"
          "ORG_ROAM_DAILIES_DIR=${cfg.orgRoamDailiesDir}"
          "LOGSEQ_GRAPH_DIR=${cfg.logseqGraphDir}"
          "ORG_LOGSEQ_SYNC_DEBOUNCE_SECONDS=${cfg.debounceSeconds}"
          "ORG_LOGSEQ_SYNC_REFRESH_ROAM=${if cfg.refreshOrgRoamDb then "1" else "0"}"
          "ORG_LOGSEQ_SYNC_EMACS_SERVER_NAME=${cfg.emacsServerName}"
        ];
      };

      Install.WantedBy = [ "default.target" ];
    };
  };
}
