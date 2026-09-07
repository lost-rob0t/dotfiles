{ config, lib, pkgs, ... }:

let
  cfg = config.vibemon;
  attemptdb = pkgs.callPackage ../../packages/attemptdb.nix { };
in
{
  options.vibemon = {
    enable = lib.mkEnableOption "VibeMon/AttemptDB coding-agent hooks";

    daemon.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Run the AttemptDB user daemon so hook events are imported continuously.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ attemptdb ];

    # AttemptDB's installer structurally merges its hooks with existing agent
    # configuration and is idempotent. It currently supports Claude Code,
    # Codex, Cursor, and Gemini CLI; only detected clients are touched.
    home.activation.vibemonHooks = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if ! ${attemptdb}/bin/attempt status >/dev/null 2>&1; then
        ${attemptdb}/bin/attempt init \
          --capture-mode metadata_only \
          --source vibemon >/dev/null
      fi

      ${attemptdb}/bin/attempt hook install \
        --scope user \
        --remove-legacy vibemon \
        --no-verify
    '';

    systemd.user.services.attemptdb = lib.mkIf cfg.daemon.enable {
      Unit = {
        Description = "AttemptDB capture daemon";
        After = [ "default.target" ];
      };

      Service = {
        ExecStart = "${attemptdb}/bin/attempt daemon run";
        Restart = "on-failure";
        RestartSec = 2;
      };

      Install.WantedBy = [ "default.target" ];
    };
  };
}
