{ config, inputs, lib, pkgs, ... }:

let
  cfg = config.zara.featureLab;
  system = pkgs.stdenv.hostPlatform.system;
  skillsPackages = lib.attrByPath [ "packages" system ] { } inputs.skills;
  opencodeWorker = skillsPackages.opencode-worker or null;
  verifierScript = "${inputs.skills}/skills/prolog-verification/scripts/prolog-verify.py";

  prologVerify = pkgs.writeShellApplication {
    name = "prolog-verify";
    runtimeInputs = [
      pkgs.brave-search-cli
      pkgs.git
      pkgs.python3
      pkgs.swi-prolog
    ];
    text = ''
      exec ${pkgs.python3}/bin/python3 ${verifierScript} "$@"
    '';
  };

  featureLab = pkgs.writeShellApplication {
    name = "zara-feature-lab";
    runtimeInputs = [
      pkgs.git
      pkgs.python3
      pkgs.swi-prolog
      prologVerify
    ] ++ lib.optional (opencodeWorker != null) opencodeWorker;
    text = ''
      export ZARA_LAB_DOTFILES_ROOT=${lib.escapeShellArg cfg.dotfilesRoot}
      export ZARA_LAB_PROLOG_RLM_ROOT=${lib.escapeShellArg cfg.prologRlmRoot}
      export ZARA_LAB_ZARA_PLUGINS_ROOT=${lib.escapeShellArg cfg.zaraPluginsRoot}
      ${lib.optionalString (cfg.workerModel != null) ''
        export ZARA_LAB_WORKER_MODEL=${lib.escapeShellArg cfg.workerModel}
      ''}
      exec ${pkgs.python3}/bin/python3 ${../../../scripts/zara-feature-lab.py} "$@"
    '';
  };

  plugin = pkgs.replaceVars ../files/zarathushtra/plugins/zara_feature_lab.py {
    labExecutable = "${featureLab}/bin/zara-feature-lab";
  };
in
{
  options.zara.featureLab = {
    enable = lib.mkEnableOption "five-worker Dotfiles Zara plugin feature lab";

    autoStart = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Automatically admit and launch the five isolated workers as a Home Manager user service.";
    };

    dotfilesRoot = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.dotfiles";
      description = "Editable Dotfiles checkout used as the feature-lab source repository.";
    };

    prologRlmRoot = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Projects/prolog-rlm";
      description = "Editable Prolog-RLM checkout whose public runtime must admit every worker.";
    };

    zaraPluginsRoot = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Projects/zara-plugins";
      description = "Editable zara-plugins checkout used only when the operator promotes a verified feature.";
    };

    workerModel = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "astra-medium";
      description = "Optional logical opencode-worker model name. Credentials remain in runtime auth, never Nix.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = opencodeWorker != null;
        message = "zara.featureLab requires inputs.skills.packages.<system>.opencode-worker.";
      }
      {
        assertion = builtins.pathExists verifierScript;
        message = "zara.featureLab requires the pinned prolog-verification skill.";
      }
    ];

    home.packages = [ featureLab ];

    zara.plugins.discoveryFiles."zara_feature_lab.py" = plugin;

    systemd.user.services.zara-feature-lab = lib.mkIf cfg.autoStart {
      Unit = {
        Description = "Five-worker Zara symbolic feature lab";
        After = [ "default.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${featureLab}/bin/zara-feature-lab start";
        ExecStop = "${featureLab}/bin/zara-feature-lab stop";
        RemainAfterExit = true;
        KillMode = "control-group";
        TimeoutStartSec = 180;
        TimeoutStopSec = 30;
        UMask = "0077";
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
