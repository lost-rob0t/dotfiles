{ config, lib, pkgs, ... }:

## Services go into here


{
  ## Xserver config
  services.xserver = {
    enable = true;
    displayManager = {
      sddm.enable = true;
      defaultSession = "lxqt";
      };
    desktopManager.lxqt.enable = true;
    windowManager.qtile.enable = true;
    videoDrivers = [ "amdgpu" ];
    layout = "us";
  };
  virtualisation = {
    podman = {
      enable = true;
      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;
    };
    libvirtd = {
      enable = true;
      onBoot = "start";
    };};

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.tor.enable = true;
  services.printing.enable = false;

  services.jellyfin = {
    enable = true;
  };
  services.i2p = {
    enable = true;
  };
}
