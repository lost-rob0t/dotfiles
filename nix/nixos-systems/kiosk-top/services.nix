{ config, lib, pkgs, ... }:

## Services go into here


{

  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;
  services = {
    ## Xserver config

    xserver = {
      libinput.enable = true;

      enable = true;
      displayManager = {
        autoLogin = {
          user = "kiosk";
          enable = true;
        };
      };
      desktopManager = {
        lxqt.enable = true;

      };
      layout = "us";
    };

    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;
      startWhenNeeded = true;
    };
    #tor = {
    #  enable = true;
    #};
    #services.i2p = {
    #  enable = true;
    #};

    # syncthing = {
    #   enable = true;
    #   user = "unseen";
    #   dataDir = "/home/unseen/Documents"; # Default folder for new synced folders
    #   configDir = "/home/unseen/Documents/.config/syncthing"; # Folder for Syncthing's settings and keys
    # };

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
