# Reusable hibernation support module.
#
# Enables hibernation by:
#   - setting the kernel `resume=` and `resume_offset=` parameters
#   - adding `powersave` to kernel modules for S3/S4 support
#
# The resume device and offset are derived from the swap configuration.
# When using a Btrfs swapfile, the offset is the physical offset
# returned by `filefrag -e /swap/swapfile` (in 4096-byte pages).
#
# Options:
#   logos.hibernation.enable     - bool, default false
#   logos.hibernation.resumeDevice - path to the swap device/file
#   logos.hibernation.resumeOffset - physical offset (pages) for swapfiles
#
# Usage in a host:
#   { logos.hibernation.enable = true; }
#
# The install-logos wrapper sets these via --system-config when
# --swap-hibernate is passed, so the module is also reusable for
# future hosts that declare their own swap.
{ config, lib, ... }:

let
  cfg = config.logos.hibernation;
in
{
  options.logos.hibernation = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable hibernation support. Requires a swap device or swapfile
        large enough to hold the contents of RAM (at least equal to
        total system memory). Set resumeDevice and resumeOffset
        accordingly.
      '';
    };

    resumeDevice = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = ''
        The device path or label of the swap area to resume from.
        For a swapfile, this is the underlying partition
        (e.g. /dev/mapper/cryptroot).
      '';
    };

    resumeOffset = lib.mkOption {
      type = lib.types.ints.unsigned;
      default = 0;
      description = ''
        Physical offset of the swapfile in the filesystem, in
        4096-byte pages. Obtain with:
          filefrag -e /swap/swapfile | awk '{print $3}'
        For a swap partition, set this to 0.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Kernel resume parameters — the kernel reads these at boot
    # to resume from the hibernation image stored in swap.
    boot.kernelParams =
      lib.optional (cfg.resumeDevice != "") "resume=${cfg.resumeDevice}"
      ++ lib.optional (cfg.resumeOffset > 0) "resume_offset=${toString cfg.resumeOffset}";

    # Allow hibernation (S4) via systemd-logind.
    systemd.sleep.extraConfig = ''
      AllowHibernation=yes
      AllowSuspendThenHibernate=yes
      HibernateMode=platform shutdown
      HibernateState=disk
    '';

    # powersave module for proper S3/S4 transitions on some firmware.
    boot.kernelModules = [ "powersave" ];
  };
}
