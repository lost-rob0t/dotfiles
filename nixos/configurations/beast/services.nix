{ config, lib, pkgs, ... }:

## Services go into here
let
  unseen = import <unseen> { config.allowUnfree = true; };

in
{
  # Heres how you run a difrent service
  #imports = [ <unseen/nixos/modules/services/networking/i2p.nix> <unseen/nixos/modules/services/networking/tor.nix> ];
  #disabledModules = [ "services/networking/i2p.nix" "services/networking/tor.nix" ];
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
  geoclue2 = {
    enable = true;
  };
  fstrim = {
    enable = true;
  };

  mastodon = {
    enable = false;
    webPort = "8090";
  };
  lighttpd = {
    enable = false;
    port = 1488;
  };};
virtualisation = {
    podman = {
      enable = false;
      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = false;
    };
    docker = {
      enable = true;
      #autoPrune = {
      #  enabled = true;
      #  dates = "weekly";
      #};

    };

    libvirtd = {
      enable = true;
      onBoot = "start";
    };
};
virtualisation.docker.autoPrune.enable = true;
}
