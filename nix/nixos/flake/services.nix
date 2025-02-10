{ inputs, config, lib, pkgs, ... }:

## Services go into here
# let
#   unseen = import <unseen> { config.allowUnfree = true; };

# in
{
  # Heres how you run a difrent service
  #imports = [ <unseen/nixos/modules/services/networking/i2p.nix> <unseen/nixos/modules/services/networking/tor.nix> ];
  #disabledModules = [ "services/networking/i2p.nix" "services/networking/tor.nix" ];
  services = {
    ## Xserver config
    displayManager = {
        sddm.enable = true;
    };
    xserver = {
      enable = true;
            # desktopManager = {
      #   lxqt.enable = false;
      #   plasma5 = {
      #     enable = true;
      #     mobile.enable = false; #maybe for Neptune touchscreen?
      #   };
      # };
      # windowManager = {
      #   exwm = {
      #     enable = true;
      #     loadScript = ''
      #       (require 'exwm)
      #       ;(load (expand-file-name "~/.dotfiles/exwm/exwm.el"))
      #     '';
      #   };
      #   # qtile = {
      #   #   enable = true;
      #   #   package = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.qtile;
      #   # };
      #   session = [{
      #     name = "qtile";
      #     start = ''
      #       ${inputs.nixpkgs-stable.legacyPackages.x86_64-linux.qtile}/bin/qtile start -b x11 \
      #       --config  /home/unseen/.config/qtile/config.py &
      #       waitPID=$!
      #     '';
      #   }];

      # };

      videoDrivers = [ "amdgpu" ];
      xkb.layout = "us";
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
      enable = false;
    };
    i2p = {
      enable = true;
    };

    ## Power managment buggy gpu :(
    tlp = {
      enable = false;
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

    flatpak = {
      # used for latest nyxt
      enable = false;
    };
    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      audio = {
        enable = true;
      };
    };
    # TODO add scripts here
    sunshine = {
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
    };

    libvirtd = {
      enable = true;
      onBoot = "start";
    };
    waydroid = {
      enable = false;
    };

  };
  virtualisation.docker.autoPrune.enable = true;
}
