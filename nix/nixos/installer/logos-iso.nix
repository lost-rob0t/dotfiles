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
  image.baseName = lib.mkForce "logos-installer";

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Graphical live session: LXQt on SDDM, autologin as the live user so the
  # install-logos launcher is reachable from the desktop without interaction.
  services = {
    openssh.enable = true;

    xserver.enable = true;
    xserver.desktopManager.lxqt.enable = true;

    displayManager = {
      sddm.enable = true;
      sddm.autoNumlock = true;
      defaultSession = "lxqt";
      autoLogin = {
        enable = true;
        user = "nixos";
      };
    };
  };

  # Live user lands on the desktop and can run the installer without a password.
  security.sudo.wheelNeedsPassword = false;

  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkForce false;

  hardware.enableRedistributableFirmware = true;

  # The full flake is materialised on the ISO so disko-install can target
  # /etc/logos#logos without needing network access to clone the repo.
  environment.etc."logos".source = self.outPath;

  environment.systemPackages = with pkgs; [
    installer
    installerGui
    installerDesktop

    firefox
    lxqt.qterminal
    git
    curl
    wget
    jq
    btrfs-progs
    cryptsetup
    dosfstools
    gptfdisk
    parted
    rsync
    util-linux
    usbutils
    pciutils
    nvme-cli
    smartmontools
  ];

  # Surface the launcher on the LXQt desktop (pcmanfm-qt renders *.desktop
  # files in ~/Desktop as icons). activationScripts runs before SDDM starts.
  system.activationScripts.installerDesktop = ''
    mkdir -p /home/nixos/Desktop
    chown nixos:users /home/nixos /home/nixos/Desktop
    ln -sfT \
      ${installerDesktop}/share/applications/logos-installer.desktop \
      /home/nixos/Desktop/logos-installer.desktop
  '';

  system.stateVersion = "26.05";
}
