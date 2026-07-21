{ lib, config, ... }:

let
  btrfsOptions = [
    "compress=zstd:3"
    "discard=async"
    "noatime"
    "space_cache=v2"
  ];

  cfg = config.logos.disk.swap;
in
{
  options.logos.disk.swap = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Create a Btrfs swapfile subvolume during disko provisioning.
        The swapfile is sized by logos.disk.swap.size.
      '';
    };

    size = lib.mkOption {
      type = lib.types.str;
      default = "8G";
      description = ''
        Swapfile size (e.g. "8G", "16G"). For hibernation, this must
        be at least as large as total system RAM.
      '';
    };

    hibernate = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable hibernation support. Implies enable=true and requires
        size >= total RAM. Sets logos.hibernation options automatically.
      '';
    };
  };

  config = {
    # Declarative disk layout for logos. disko owns partitioning, LUKS,
    # Btrfs subvolumes, and the resulting fileSystems/boot.initrd.luks.devices
    # entries. The device is overwritten by `disko-install --disk main <dev>`
    # at install time, so this default is only a placeholder.
    disko.devices = {
      disk = {
        main = {
          type = "disk";
          device = lib.mkDefault "/dev/disk/by-id/LOGOS-INSTALL-TARGET";
          content = {
            type = "gpt";
            partitions = {
              ESP = {
                size = "1G";
                type = "EF00";
                content = {
                  type = "filesystem";
                  format = "vfat";
                  mountpoint = "/boot";
                  mountOptions = [
                    "fmask=0077"
                    "dmask=0077"
                  ];
                };
              };

              cryptroot = {
                size = "100%";
                content = {
                  type = "luks";
                  name = "cryptroot";
                  # No keyFile -> interactive passphrase prompt at install
                  # time. Set via LOGOS_LUKS_PASSWORD_FILE for non-interactive.
                  settings = {
                    allowDiscards = true;
                  };
                  content = {
                    type = "btrfs";
                    extraArgs = [
                      "-f"
                      "-L"
                      "logos"
                    ];
                    subvolumes = {
                      "@" = {
                        mountpoint = "/";
                        mountOptions = btrfsOptions;
                      };
                      "@home" = {
                        mountpoint = "/home";
                        mountOptions = btrfsOptions;
                      };
                      "@nix" = {
                        mountpoint = "/nix";
                        mountOptions = btrfsOptions;
                      };
                      "@log" = {
                        mountpoint = "/var/log";
                        mountOptions = btrfsOptions;
                      };
                      "@snapshots" = {
                        mountpoint = "/.snapshots";
                        mountOptions = btrfsOptions;
                      };
                    } // lib.optionalAttrs cfg.enable {
                      "@swap" = {
                        mountpoint = "/.swapvol";
                        mountOptions = [ "noatime" ];
                        swap.swapfile.size = cfg.size;
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };

    # When hibernation is requested, wire the resume parameters to the
    # reusable hibernation module. The swapfile lives on the encrypted
    # Btrfs root, so resumeDevice is the mapper device.
    logos.hibernation = lib.mkIf cfg.hibernate {
      enable = true;
      resumeDevice = "/dev/mapper/cryptroot";
      # The offset is computed at install time by the install-logos
      # wrapper (filefrag) and passed via --system-config. This default
      # of 0 means "partition swap" — swapfiles override it.
      resumeOffset = 0;
    };
  };
}
