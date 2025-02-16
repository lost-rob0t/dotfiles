# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.unseen = {
    isNormalUser = true;
    extraGroups = [ "wheel" "libvirtd" "adbusers" "docker" "networkmanager" ]; # Enable ‘sudo’ for the user.
  };



  environment.variables.DICPATH = "/run/current-system/sw/share/hunspell:/run/current-system/sw/share/hyphen";
  system.autoUpgrade = {
    flake = "github:lost-rob0t/dotfiles";
    enable = true;
    dates = "weekly";
  };

  # NOTE This will require me to make sure this is always up.
  # If its down will it prevent my system from booting?
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).

  system.stateVersion = "21.11"; # Did you read the comment?
 # boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  #systemd.extraConfig = "DefaultLimitNOFILE=8096:524288";
  #
}
