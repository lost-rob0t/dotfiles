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
    libinput.enable = true; # enable touch support
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
    videoDrivers = [ "modesetting" ]; #  change this for laptop # TODO see if i can re add amdgpu
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
    enable = false;
  };

  blueman = {
    enable = true;
  };
  ## Power managment buggy gpu :(
  tlp = {
  enable = true;
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
  geoclue2 = {
    enable = true;
  };
  fstrim = {
    enable = true;
  };

  lighttpd = {
    enable = false;
    port = 1488;
  };
  flatpak = {
    # used for latest nyxt
    enable = true;
  };
  };
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
      enable = false;
      onBoot = "start";
    };
};
virtualisation.docker.autoPrune.enable = true;
}
