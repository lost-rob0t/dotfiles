{ config, lib, pkgs, ... }:
let
  cfg = config.kbRoam;

  mkCli = name: script: runtimeInputs: extra: pkgs.writeShellApplication {
    inherit name runtimeInputs;
    text = ''
      export KB_ROAM_ELISP='\${../../../lisp/wiki/kb-roam.el}'
      export KB_ROAM_CENSOR_PL='\${../../../lisp/wiki/roam-censor.pl}'
      export KB_ROAM_ASSET_DIR='\${../../../lisp/wiki/assets}'
      \${extra}
      exec '\${pkgs.bash}/bin/bash' '\${script}' "$@"
    '';
  };

  kbIngest = mkCli "kb-ingest" ../../../scripts/kb-ingest
    [ pkgs.coreutils pkgs.emacs ]
    "";

  sessionExport = mkCli "opencode-session-to-org" ../../../scripts/opencode-session-to-org
    [ pkgs.coreutils pkgs.emacs pkgs.jq config.opencode.package ]
    "";

  roamCensor = mkCli "roam-censor" ../../../scripts/roam-censor
    [ pkgs.coreutils pkgs.emacs pkgs.swiProlog ]
    "";

  roamPublish = mkCli "roam-publish" ../../../scripts/roam-publish
    [ pkgs.coreutils pkgs.emacs pkgs.swiProlog ]
    "export ROAM_CENSOR_CMD='\${roamCensor}/bin/roam-censor'";

  roamCheck = mkCli "check-roam-publish" ../../../scripts/check-roam-publish
    [ pkgs.coreutils pkgs.emacs pkgs.swiProlog pkgs.shellcheck ]
    "";
in
{
  options.kbRoam = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install audited Org-roam AI ingestion, censoring, and publishing when OpenCode is enabled.";
    };

    notesRoot = lib.mkOption {
      type = lib.types.str;
      default = "\${config.home.homeDirectory}/Documents/Notes/org";
      description = "Canonical Org-roam root.";
    };

    publishRoot = lib.mkOption {
      type = lib.types.str;
      default = "\${config.xdg.stateHome}/kb-roam/site";
      description = "Generated public site output.";
    };

    baseUrl = lib.mkOption {
      type = lib.types.str;
      default = "/";
      description = "Base URL used by the generated static site.";
    };
  };

  config = lib.mkIf (cfg.enable && config.opencode.enable) {
    home.packages = [ kbIngest sessionExport roamCensor roamPublish roamCheck ];

    home.sessionVariables = {
      KB_ROAM_ROOT = cfg.notesRoot;
      KB_ROAM_PUBLISH_ROOT = cfg.publishRoot;
      ROAM_PUBLISH_BASE_URL = cfg.baseUrl;
    };

    xdg.configFile."opencode/agents/kb-ingest.md".source =
      ../files/opencode-agents/kb-ingest.md;
    xdg.configFile."opencode/agents/kb-audit.md".source =
      ../files/opencode-agents/kb-audit.md;
    xdg.configFile."kb-roam/kb-roam.el".source =
      ../../../lisp/wiki/kb-roam.el;
    xdg.configFile."kb-roam/roam-censor.pl".source =
      ../../../lisp/wiki/roam-censor.pl;
    xdg.configFile."kb-roam/assets/roam.css".source =
      ../../../lisp/wiki/assets/roam.css;
  };
}
