{ lib, ... }:

let
  btrfsOptions = [
    "compress=zstd:3"
    "discard=async"
    "noatime"
    "space_cache=v2"
  ];
in
{
  # Declarative disk layout for logos. disko owns partitioning, LUKS, Btrfs
  # subvolumes, and the resulting fileSystems/boot.initrd.luks.devices
  # entries. The device is overwritten by `disko-install --disk main <dev>`
  # at install time, so this default is only a placeholder.
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = lib.mkDefault "/dev/disk/by-id/REPLACE-ME";
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
                # No keyFile -> interactive passphrase prompt at install time,
                # matching the previous provision-logos-disk.sh behavior.
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
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
