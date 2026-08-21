{ config, lib, pkgs, ... }:
let
  cfg = config.homeManagerUpdater;
  configuration = "${config.home.username}@${cfg.hostName}";
  dataDir = "${config.home.homeDirectory}/.local/share/home-manager-updater";
  logrotateConfig = pkgs.writeText "home-manager-updater-logrotate.conf" ''
    ${dataDir}/update.log {
      daily
      rotate 14
      compress
      delaycompress
      missingok
      notifempty
      dateext
    }
  '';
  updater = pkgs.writeShellApplication {
    name = "home-manager-updater";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.git
      pkgs.gh
      pkgs.gnused
      pkgs.jq
      pkgs.libnotify
      pkgs.logrotate
      pkgs.nix
      config.programs.home-manager.package
    ];
    text = ''
      exec ${pkgs.bash}/bin/bash ${../../../scripts/home-manager-updater.sh}
    '';
  };
  notifyFailure = pkgs.writeShellApplication {
    name = "home-manager-updater-notify-failure";
    runtimeInputs = [ pkgs.coreutils pkgs.libnotify ];
    text = ''
      failure_file=${lib.escapeShellArg "${dataDir}/failure.txt"}
      id_file=${lib.escapeShellArg "${dataDir}/notification-id"}
      if [[ ! -s "$failure_file" ]]; then
        exit 0
      fi
      if [[ -s "$id_file" ]]; then
        old_id="$(cat "$id_file")"
        notification_id="$(notify-send --print-id --replace-id="$old_id" \
          --urgency=critical --expire-time=0 \
          --app-name="Home Manager Updater" \
          "Home Manager auto-update failed" \
          "$(cat "$failure_file")")"
      else
        notification_id="$(notify-send --print-id --urgency=critical --expire-time=0 \
          --app-name="Home Manager Updater" \
          "Home Manager auto-update failed" \
          "$(cat "$failure_file")")"
      fi
      printf '%s\n' "$notification_id" >"$id_file"
    '';
  };
in
{
  options.homeManagerUpdater = {
    enable = lib.mkEnableOption "automatic Home Manager updates from a flake";

    hostName = lib.mkOption {
      type = lib.types.str;
      description = "Host component used to derive the <user>@<host> Home Manager configuration name.";
      example = "flake";
    };

    repository = lib.mkOption {
      type = lib.types.str;
      default = "lost-rob0t/dotfiles";
      description = "GitHub repository containing the Home Manager flake.";
    };

    branch = lib.mkOption {
      type = lib.types.str;
      default = "master";
      description = "Branch fetched by the updater.";
    };

    schedule = lib.mkOption {
      type = lib.types.str;
      default = "03:00:00";
      description = "Local systemd calendar time for the daily updater run.";
    };

    randomizedDelaySec = lib.mkOption {
      type = lib.types.str;
      default = "30m";
      description = "Maximum randomized delay applied to the daily updater timer.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ updater ];

    systemd.user.services.home-manager-updater = {
      Unit = {
        Description = "Update and activate Home Manager configuration ${configuration}";
        OnFailure = [ "home-manager-updater-failure-notification.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${updater}/bin/home-manager-updater";
        Environment = [
          "HM_UPDATER_DATA_DIR=${dataDir}"
          "HM_UPDATER_REPOSITORY=${cfg.repository}"
          "HM_UPDATER_REMOTE_URL=https://github.com/${cfg.repository}.git"
          "HM_UPDATER_BRANCH=${cfg.branch}"
          "HM_UPDATER_CONFIGURATION=${configuration}"
          "HM_UPDATER_LOGROTATE_CONFIG=${logrotateConfig}"
          "GIT_TERMINAL_PROMPT=0"
        ];
      };
    };

    systemd.user.timers.home-manager-updater = {
      Unit.Description = "Daily Home Manager updater for ${configuration}";
      Timer = {
        OnCalendar = "*-*-* ${cfg.schedule}";
        Persistent = true;
        RandomizedDelaySec = cfg.randomizedDelaySec;
        AccuracySec = "5m";
        Unit = "home-manager-updater.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };

    systemd.user.services.home-manager-updater-failure-notification = {
      Unit = {
        Description = "Persistent Home Manager updater failure notification";
        After = [ "graphical-session.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecCondition = "${pkgs.coreutils}/bin/test -s ${dataDir}/failure.txt";
        ExecStart = "${notifyFailure}/bin/home-manager-updater-notify-failure";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
