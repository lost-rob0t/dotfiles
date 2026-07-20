{
  self,
  lib,
  pkgs,
  modulesPath,
  ...
}:

let
  system = pkgs.stdenv.hostPlatform.system;
  installer = self.packages.${system}.install-logos;
  installerGui = self.packages.${system}.install-logos-gui;
  installerDesktop = pkgs.makeDesktopItem {
    name = "logos-installer";
    desktopName = "Install Logos";
    comment = "Install Logos with encrypted Btrfs";
    exec = "${installerGui}/bin/install-logos-gui";
    icon = "system-software-install";
    terminal = false;
    categories = [ "System" ];
  };
in
{
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-graphical-base.nix")
  ];

  networking.hostName = "logos-installer";
  isoImage.isoBaseName = lib.mkForce "logos-installer";

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  services = {
    openssh.enable = true;

    xserver.desktopManager.lxqt.enable = true;

    displayManager = {
      sddm.enable = true;
      defaultSession = "lxqt";
      autoLogin = {
        enable = true;
        user = "nixos";
      };
    };
  };

  security.sudo.wheelNeedsPassword = false;

  environment = {
    etc."logos".source = self.outPath;

    systemPackages = with pkgs; [
      btrfs-progs
      cryptsetup
      dosfstools
      git
      gptfdisk
      jq
      parted
      rsync
      util-linux
      installer
      installerGui
      installerDesktop
    ];
  };

  system.activationScripts.installerDesktop = ''
    mkdir -p /home/nixos/Desktop
    chown nixos:users /home/nixos /home/nixos/Desktop
    ln -sfT \
      ${installerDesktop}/share/applications/logos-installer.desktop \
      /home/nixos/Desktop/logos-installer.desktop
  '';

  system.stateVersion = "26.05";
}
