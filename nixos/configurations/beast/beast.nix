# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixFlakes; # or versioned attributes like nix_2_7
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };
  nixpkgs.config.allowUnfree = true;


  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./packages.nix
      ./services.nix
      ./networking.nix
      ./security.nix
    ];

  # Boot config
  boot.initrd.luks.devices = {
  crypted = {
    device = "/dev/disk/by-partuuid/e530f416-662e-4e97-a698-63096214c5f9";
    allowDiscards = true; # Used if primary device is a SSD
    preLVM = true;
    bypassWorkqueues = true;
  };
 };
  boot.loader.systemd-boot.enable = true;
 # Set your time zone.
  time.timeZone = "America/New_York";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.unseen = {
    isNormalUser = true;
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
    extraGroups = [ "wheel" "libvirtd" "adbusers" ]; # Enable ‘sudo’ for the user.
  };
  environment.pathsToLink = [ "/share/hunspell" "/share/myspell" "/share/hyphen" ];

  environment.variables.DICPATH = "/run/current-system/sw/share/hunspell:/run/current-system/sw/share/hyphen";
    # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

}
