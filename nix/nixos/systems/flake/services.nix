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
    xserver = {
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

    # Remote stuff
    # Enable the OpenSSH daemon.
    xrdp = {
      enable = false;

    };
    openssh = {
      enable = true;
      #startWhenNeeded = true;

    };
    tor = {
      enable = true;
    };
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    i2p = {
      enable = false;
    };

    blueman = {
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


    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
    # TODO add scripts here
    sunshine = {
      enable = true;

    };

    open-webui = {
      enable = true;
      package = pkgs.open-webui.overrideAttrs (oldAttrs: {
      propagatedBuildInputs = (oldAttrs.propagatedBuildInputs or []) ++ (with pkgs.python3Packages; [
        aiohttp
        requests
        numpy
        pandas
        scipy
        matplotlib
        scikit-learn
        pyyaml
        jinja2
        click
        python-dateutil
        pytz
        anthropic
        pydantic
      ]);
      });


  environment = {
    ANONYMIZED_TELEMETRY = "False";
    DO_NOT_TRACK = "True";
    SCARF_NO_ANALYTICS = "True";
    OLLAMA_API_BASE_URL = "http://127.0.0.1:11434";
    # Enable function calling/tools support
    ENABLE_FUNCTION_CALLING = "true";
    BACKEND_CORS_ORIGINS = ''"*"'';
  };
};
     ollama = {
       enable = true;
       package = pkgs.ollama-rocm;
       openFirewall = true;
       host = "0.0.0.0";
       user = "ollama";
       group = "ai";
       home = "/home/ollama";
  };};
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
