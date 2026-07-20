{
  self,
  disko,
  lib,
  pkgs,
  modulesPath,
  ...
}:

let
  disko-install = disko.packages.${pkgs.system}.disko-install;

  install-logos-desktop = pkgs.writeTextFile {
    name = "install-logos-desktop";
    destination = "/share/applications/install-logos.desktop";
    text = ''
      [Desktop Entry]
      Type=Application
      Name=Install Logos
      GenericName=NixOS Installer
      Comment=Provision and install logos NixOS on a target disk
      Exec=sudo install-logos
      Icon=drive-harddisk
      Terminal=true
      Categories=System;Installation;
      Keywords=nixos;install;disko;
    '';
  };

  install-logos = pkgs.writeShellScriptBin "install-logos" ''
    exec ${self.outPath}/scripts/install-logos.sh "$@"
  '';
in
{
  imports = [
    (modulesPath + "/installer/cd-dvd/installation-cd-minimal.nix")
  ];

  networking.hostName = "logos-installer";
  image.baseName = lib.mkForce "logos-installer";

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Graphical live session: LXQt on SDDM, autologin as the live user so the
  # install-logos launcher is reachable from the desktop without interaction.
  services.xserver.enable = true;
  services.xserver.desktopManager.lxqt.enable = true;
  services.displayManager.sddm = {
    enable = true;
    autoNumlock = true;
  };
  services.displayManager.autoLogin = {
    enable = true;
    user = "nixos";
  };
  services.displayManager.defaultSession = "lxqt";

  # Live user lands on the desktop and can run the installer without a password.
  security.sudo.wheelNeedsPassword = false;

  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkForce false;

  services.openssh.enable = true;

  hardware.enableRedistributableFirmware = true;

  # The full flake is materialised on the ISO so disko-install can target
  # /etc/logos#logos without needing network access to clone the repo.
  environment.etc."logos".source = self.outPath;

  environment.systemPackages = with pkgs; [
    disko-install
    install-logos
    install-logos-desktop

    firefox
    lxqt.qterminal
    git
    curl
    wget
    btrfs-progs
    cryptsetup
    dosfstools
    gptfdisk
    parted
    util-linux
    usbutils
    pciutils
    nvme-cli
    smartmontools
  ];

  # Surface the launcher on the LXQt desktop (pcmanfm-qt renders *.desktop
  # files in ~/Desktop as icons). tmpfiles runs before SDDM starts.
  systemd.tmpfiles.rules = [
    "d /home/nixos/Desktop 0755 nixos users -"
    "L+ /home/nixos/Desktop/install-logos.desktop - - - - ${install-logos-desktop}/share/applications/install-logos.desktop"
  ];

  system.stateVersion = "26.05";
}
