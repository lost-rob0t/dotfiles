{ config, lib, pkgs, ... }:

## Services go into here


{
  services = {
  ## Xserver config
  xserver = {
    enable = true;
    displayManager = {
      sddm.enable = true;
      defaultSession = "lxqt";
      };
    desktopManager = {
      lxqt.enable = true;
      mate.enable = true;
    };
    windowManager.qtile.enable = true;
    videoDrivers = [ "amdgpu" ];
    layout = "us";
  };

  # Enable the OpenSSH daemon.
  openssh = {
    enable = true;
    startWhenNeeded = true;

  };
  tor = {
    enable = true;
  };
  printing = {
    enable = false;};
  #services.i2p = {
  #  enable = true;
  #};
  emacs = {
    enable = true;
    defaultEditor = true;
  };
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
    };
};
}
