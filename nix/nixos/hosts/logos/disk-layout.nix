{ lib, ... }:

let
  btrfsOptions = [
    "compress=zstd:3"
    "discard=async"
    "noatime"
  ];
in
{
  disko.devices.disk.main = {
    type = "disk";
    device = lib.mkDefault "/dev/disk/by-id/LOGOS-INSTALL-TARGET";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          label = "EFI";
          size = "1G";
          type = "EF00";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };

        cryptroot = {
          label = "cryptroot";
          size = "100%";
          content = {
            type = "luks";
            name = "cryptroot";
            settings.allowDiscards = true;
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
}
