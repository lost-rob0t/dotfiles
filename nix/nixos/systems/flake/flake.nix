# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./packages.nix
      ./services.nix
      ./networking.nix
      ./security.nix
      ./misc.nix
    ];

  # Boot config
  boot.loader.systemd-boot.enable = true;
  # Set your time zone.
  time.timeZone = "America/New_York";

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with 'passwd'.
  users.users.unseen = {
    isNormalUser = true;
    extraGroups = [ "wheel" "libvirtd" "adbusers" "docker" "networkmanager" ]; # Enable 'sudo' for the user.
  };

  # Desktop configuration
  desktop = {
    enable = true;
    sessionType = "x11";
    bluetooth.enable = true;
    fonts.enable = true;
    qtile = {
      enable = true;
      extraPackages = with pkgs; [
        rofi
        dunst
        libnotify
        picom
        feh
        networkmanagerapplet
        volumeicon
      ];
    };
  };

  # X11 Configuration
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+qtile";
    };
  };

  # Bluetooth Configuration
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  environment.variables.DICPATH = "/run/current-system/sw/share/hunspell:/run/current-system/sw/share/hyphen";

  system.stateVersion = "21.11"; # Did you read the comment?
}