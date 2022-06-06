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
        user = "unseen";
        enable = true;
      };
      gdm.enable = true;
      defaultSession = "gnome";
      };
    desktopManager = {
      lxqt.enable = true;
      gnome.enable = true;

    };
    windowManager.qtile.enable = true;
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
  blueman = {
    enable = true;
  };

 touchegg = {
 enable = true;
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
