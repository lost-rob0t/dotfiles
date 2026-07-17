{ self, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
  ];

  networking.hostName = "logos-installer";
  isoImage.isoBaseName = lib.mkForce "logos-installer";

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  services.openssh.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment = {
    etc."logos".source = self.outPath;

    systemPackages = with pkgs; [
      btrfs-progs
      cryptsetup
      dosfstools
      git
      gptfdisk
      parted
      rsync
      util-linux

      (writeShellScriptBin "provision-logos" ''
        exec ${self.outPath}/scripts/provision-logos-disk.sh "$@"
      '')

      (writeShellScriptBin "install-logos" ''
        exec ${self.outPath}/scripts/install-logos.sh "$@"
      '')
    ];
  };

  system.stateVersion = "26.05";
}
