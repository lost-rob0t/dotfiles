{ config, lib, pkgs, ... }:

## Services go into here


{
  services = {
  ## Xserver config
  xserver = {
    enable = true;
    displayManager = {
      sddm.enable = true;
      };
    desktopManager = {
      lxqt.enable = true;
      plasma5 = {
        enable = false;
        mobile.enable = false; #maybe for Neptune touchscreen?
      };
    };
    windowManager.qtile.enable = true;
    videoDrivers = [ "amdgpu" ];
    layout = "us";
  };

  # Enable the OpenSSH daemon.
  openssh = {
    enable = true;
    #startWhenNeeded = true;

  };
  tor = {
    enable = true;
  };
  printing = {
    enable = false;};
  i2p = {
    enable = true;
  };
  emacs = {
    enable = true;
    defaultEditor = true;
    package = pkgs.emacsPgtk;
  };
  blueman = {
    enable = true;
  };
  ## Power managment buggy gpu :(
  tlp = {
  enable = false;
  };
  ## Open snitch
  #opensnitch = {
  #  enable = true;
  #};
  syncthing = {
    enable = true;
    user = "unseen";
    dataDir = "/home/unseen/Documents";    # Default folder for new synced folders
    configDir = "/home/unseen/Documents/.config/syncthing";   # Folder for Syncthing's settings and keys
    };
  gvfs = {
    enable = true;
  };
  radicale = {
  enable = false;
  settings = {
    server.hosts = [ "0.0.0.0:5232" ];
    auth = {
      type = "htpasswd";
      htpasswd_filename = "/admin/htpasswd/radical_httppassword";
      # hash function used for passwords. May be `plain` if you don't want to hash the passwords
      htpasswd_encryption = "bcrypt";
    };
  };
};
  fstrim = {
    enable = true;
  };

  mastodon = {
    enable = false;
    webPort = "8090";
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
