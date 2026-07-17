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
  boot.initrd.luks.devices.cryptroot.device =
    lib.mkForce "/dev/disk/by-partlabel/cryptroot";

  fileSystems = {
    "/" = lib.mkForce {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
      options = btrfsOptions ++ [ "subvol=@" ];
    };

    "/home" = lib.mkForce {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
      options = btrfsOptions ++ [ "subvol=@home" ];
    };

    "/nix" = lib.mkForce {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
      options = btrfsOptions ++ [ "subvol=@nix" ];
    };

    "/var/log" = lib.mkForce {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
      options = btrfsOptions ++ [ "subvol=@log" ];
    };

    "/.snapshots" = lib.mkForce {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
      options = btrfsOptions ++ [ "subvol=@snapshots" ];
    };

    "/boot" = lib.mkForce {
      device = "/dev/disk/by-partlabel/EFI";
      fsType = "vfat";
      options = [
        "fmask=0077"
        "dmask=0077"
      ];
    };
  };

  swapDevices = lib.mkForce [ ];
}
