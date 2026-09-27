{ config, lib, pkgs, ... }:
let
  cfg = config.zara;
  enabled = cfg.coding.enable || cfg.scheduled.enable;
  python = pkgs.python3.withPackages (p: [ p.tomlkit ]);
  settings = {
    tasks = {
      enabled = true;
      max_concurrent = cfg.scheduled.maxConcurrent;
      max_task_steps = cfg.scheduled.maxTaskSteps;
      wall_clock_minutes = cfg.scheduled.wallClockMinutes * 1.0;
      step_log_chars = cfg.scheduled.stepLogChars;
    };
  } // lib.optionalAttrs cfg.coding.enable {
    plugins.zara-coding = {
      allowed_roots = cfg.coding.allowedRoots;
      prolog_rlm_checkout = cfg.coding.prologRlmCheckout;
      git = "${pkgs.git}/bin/git";
      swipl = "${pkgs.swi-prolog}/bin/swipl";
    };
  };
  settingsFile = pkgs.writeText "zara-development-settings.json" (builtins.toJSON settings);
  checks = pkgs.runCommand "zara-development-config-tests" { nativeBuildInputs = [ python ]; } ''
    mkdir -p files mods
    cp ${../files/apply-zara-development-config.py} files/apply-zara-development-config.py
    cp ${../files/test-zara-development-config.py} files/test-zara-development-config.py
    cp ${./zara-development.org} mods/zara-development.org
    cp ${./zara-development.nix} mods/zara-development.nix
    python files/test-zara-development-config.py
    python files/apply-zara-development-config.py "$TMPDIR/config.toml" ${settingsFile}
    touch "$out"
  '';
  updater = pkgs.writeShellApplication {
    name = "apply-zara-development-config";
    runtimeInputs = [ python ];
    text = ''
      test -e ${checks}
      exec python ${../files/apply-zara-development-config.py} "$@"
    '';
  };
in
{
  options.zara = {
    coding = {
      enable = lib.mkEnableOption "Zara's existing Prolog-RLM coding plugin";
      allowedRoots = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "${config.home.homeDirectory}/Documents/Projects" "${config.home.homeDirectory}/git/worktrees" ];
        description = "Explicit repository and linked-worktree roots; never the whole filesystem.";
      };
      prologRlmCheckout = lib.mkOption {
        type = lib.types.str;
        default = "${config.home.homeDirectory}/Documents/Projects/prolog-rlm";
        description = "Existing Prolog-RLM checkout; the plugin checks runtime readiness.";
      };
    };
    scheduled = {
      enable = lib.mkEnableOption "Zara's canonical scheduled-task substrate";
      maxConcurrent = lib.mkOption { type = lib.types.ints.positive; default = 2; };
      maxTaskSteps = lib.mkOption { type = lib.types.ints.positive; default = 20; };
      wallClockMinutes = lib.mkOption { type = lib.types.ints.positive; default = 30; };
      stepLogChars = lib.mkOption { type = lib.types.ints.positive; default = 2000; };
    };
  };

  config = lib.mkIf (cfg.enable && enabled) {
    assertions = [ {
      assertion = cfg.server.enable;
      message = "Zara coding/scheduled setup requires the existing zara-server service.";
    } ];
    zara.settings = settings;
    zara.plugins.registry = lib.optional cfg.coding.enable "zara-coding";
    home.packages = [ updater ] ++ lib.optionals cfg.coding.enable [ pkgs.git pkgs.swi-prolog ];
    home.activation.zaraDevelopmentConfig = lib.mkIf (!cfg.nixManaged) (
      lib.hm.dag.entryAfter [ "writeBoundary" "zaraExpertConfig" ] ''
        $DRY_RUN_CMD ${updater}/bin/apply-zara-development-config \
          ${lib.escapeShellArg "${config.home.homeDirectory}/.config/zarathushtra/config.toml"} \
          ${settingsFile}
      ''
    );
  };
}
