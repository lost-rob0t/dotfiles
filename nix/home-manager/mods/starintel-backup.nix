{ config, lib, pkgs, ... }:

let
  cfg = config.starintelBackup;

  mountGuard = ''
    # Trigger autofs and verify the CIFS share is actually mounted
    # (findmnt -T prints one line per stacked mount: autofs + cifs).
    ls "${cfg.shareDir}" >/dev/null 2>&1 || exit 0
    if ! ${pkgs.util-linux}/bin/findmnt -n -T "${cfg.shareDir}" -o FSTYPE \
      | ${pkgs.gnugrep}/bin/grep -qx 'cifs\|smb3'; then
      exit 0
    fi
  '';

  mirrorScript = pkgs.writeShellApplication {
    name = "starintel-mirror";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.gnugrep
      pkgs.rsync
      pkgs.util-linux
    ];
    text = ''
      export SRC="${cfg.sourceDir}/"
      export DEST="${cfg.shareDir}/starintel/"
      uid=$(id -u)
      ${mountGuard}
      exec 9>/run/user/"$uid"/starintel-mirror.lock
      ${pkgs.util-linux}/bin/flock -n 9 || exit 0
      mkdir -p "$DEST"
      exec ${pkgs.rsync}/bin/rsync \
        -a --delete --delete-excluded --delay-updates --no-links \
        --exclude='__pycache__/' \
        --exclude='node_modules/' \
        --exclude='.venv/' \
        --exclude='.cache/' \
        --exclude='*.sqlite3-wal' \
        --exclude='*.sqlite3-shm' \
        --info=stats1 "$SRC" "$DEST"
    '';
  };

  backupScript = pkgs.writeShellApplication {
    name = "starintel-backup";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.gnutar
      pkgs.gzip
      pkgs.util-linux
    ];
    text = ''
      src_name=$(basename "${cfg.sourceDir}")
      src_parent=$(dirname "${cfg.sourceDir}")
      dest_dir="${cfg.shareDir}/Backups"
      stamp=$(date +%Y%m%d-%H%M%S)
      tmp="$dest_dir/.starintel-$stamp.tar.gz.tmp"
      out="$dest_dir/starintel-$stamp.tar.gz"
      uid=$(id -u)
      ${mountGuard}
      exec 9>/run/user/"$uid"/starintel-backup.lock
      ${pkgs.util-linux}/bin/flock -n 9 || exit 0
      mkdir -p "$dest_dir"
      # GC interrupted snapshots and enforce retention BEFORE writing — on a
      # full share the tar would die mid-write (learned 2026-11-02).
      find "$dest_dir" -maxdepth 1 -type f -name '.starintel-*.tar.gz.tmp' -mmin +1440 -delete
      find "$dest_dir" -maxdepth 1 -type f -name 'starintel-*.tar.gz' -mmin +${toString (cfg.retentionDays * 24 * 60)} -delete
      # Space gate: refuse to write unless ~1.5x the last snapshot is free.
      last_size=$(find "$dest_dir" -maxdepth 1 -type f -name 'starintel-*.tar.gz' -printf '%T@ %s\n' 2>/dev/null | sort -rn | head -n 1 | cut -d' ' -f2)
      [ -n "$last_size" ] || last_size=21000000000
      need=$(( last_size * 3 / 2 + 1000000000 ))
      avail=$(df -B1 --output=avail "$dest_dir" | tail -n 1)
      if [ "$avail" -lt "$need" ]; then
        echo "starintel-backup: need $need bytes free on the share, have $avail — refusing to write" >&2
        exit 1
      fi
      # Publish atomically: build as .tmp then rename into place.
      ${pkgs.gnutar}/bin/tar -czf "$tmp" -C "$src_parent" \
        --exclude="$src_name/__pycache__" \
        --exclude="$src_name/node_modules" \
        --exclude="$src_name/.cache" \
        "$src_name" \
      && mv -f "$tmp" "$out"
    '';
  };
in
{
  options.starintelBackup = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Mirror and snapshot ~/starintel to the SMB share.";
    };

    sourceDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/starintel";
      description = "Directory to mirror and back up.";
    };

    shareDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/share";
      description = "Autofs/CIFS share mount point receiving the mirror and backups.";
    };

    mirrorInterval = lib.mkOption {
      type = lib.types.str;
      default = "5m";
      description = "Interval between rsync mirror runs.";
    };

    backupInterval = lib.mkOption {
      type = lib.types.str;
      default = "3h";
      description = "Interval between tar.gz snapshot runs.";
    };

    retentionDays = lib.mkOption {
      type = lib.types.int;
      default = 3;
      description = "Rolling days of tar.gz snapshots to keep (exact minute window).";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user.services.starintel-mirror = {
      Unit = {
        Description = "Fast incremental rsync mirror of ~/starintel to the share";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${mirrorScript}/bin/starintel-mirror";
        Nice = 10;
        IOSchedulingClass = "best-effort";
        IOSchedulingPriority = 7;
      };
    };

    systemd.user.timers.starintel-mirror = {
      Unit.Description = "Mirror ~/starintel every ${cfg.mirrorInterval}";
      Timer = {
        OnBootSec = "2m";
        OnUnitActiveSec = cfg.mirrorInterval;
        AccuracySec = "30s";
        Persistent = true;
        Unit = "starintel-mirror.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };

    systemd.user.services.starintel-backup = {
      Unit = {
        Description = "Rolling tar.gz snapshot of ~/starintel on the share";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${backupScript}/bin/starintel-backup";
        Nice = 10;
        IOSchedulingClass = "best-effort";
        IOSchedulingPriority = 7;
      };
    };

    systemd.user.timers.starintel-backup = {
      Unit.Description = "Snapshot ~/starintel every ${cfg.backupInterval}";
      Timer = {
        OnBootSec = "10m";
        OnUnitActiveSec = cfg.backupInterval;
        AccuracySec = "1m";
        Persistent = true;
        Unit = "starintel-backup.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
