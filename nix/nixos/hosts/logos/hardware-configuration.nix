{ config, lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.extraModulePackages = [ ];

  boot.initrd.luks.devices = {
    dm-root.device = "/dev/disk/by-uuid/552dc146-d779-4831-a648-e7b207780e0b";
    dm-home.device = "/dev/disk/by-uuid/d2c146aa-ceef-4a10-a6aa-fdb8fb9f85ac";
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/0b67f502-a60c-415b-8ffe-81549c99a207";
      fsType = "btrfs";
      options = [ "compress=zstd:1" ];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/1a66e715-70ef-4fc1-8375-5d697f905435";
      fsType = "btrfs";
      options = [ "compress=zstd:2" ];
    };

    "/boot" = {
      device = "/dev/disk/by-partuuid/e5abfa14-a752-47ff-8ca6-c146445e6ced";
      fsType = "vfat";
      options = [
        "uid=0"
        "gid=0"
        "fmask=0077"
        "dmask=0077"
      ];
    };

    "/home/unseen/share" = {
      device = "//storage.lost.system/unseen";
      fsType = "cifs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=60"
        "x-systemd.device-timeout=5s"
        "x-systemd.mount-timeout=5s"
        "uid=1000"
        "credentials=/home/unseen/.config/smb-creds"
      ];
    };
  };

  swapDevices = [ ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
