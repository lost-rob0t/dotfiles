{ inputs, outputs, lib, config, pkgs, ... }:
{

  imports = [
    ./../../mods/default.nix
    ./programs.nix
  ];

  nixpkgs = {

    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = (_: true);
    };
  };

  llm = {
    enable = true;
  };

  opencode.web = {
    enable = true;
    hostname = "127.0.0.1";
    port = 4096;
  };

  hackmode.enable = true;

  # Zara runtime is temporarily disabled while the stack is cleaned up.
  # Keep the declarative values in place so re-enabling remains deliberate.
  zara = {
    enable = false;
    nixManaged = false;
    workflows.enable = false;

    server = {
      enable = false;
      environmentFile = "-%h/.config/zarathushtra/secrets.env";
    };

    desktop.enable = false;
    wake.enable = false;

    plugins = {
      enableAll = false;
      discoveryFiles = {
        "starintel.py" = ../../files/zarathushtra/plugins/starintel.py;
        "lyria.py" = ../../files/zarathushtra/plugins/lyria.py;
      };
    };
  };

  # Qwen3-TTS is temporarily disabled because it may cause GPU crashes or
  # wider UI/system instability. Reassess it before uncommenting this block.
  # services.qwen3-tts = {
  #   enable = true;
  #   autoStart = true;
  #   backend = "vulkan";
  #   port = 7860;
  # };

  # Local clients (wake listener, CLI, desktop) share Zara's owner-private
  # IPC endpoint; config.toml's [daemon] endpoint stays empty so the client
  # falls back to the same socket. Remote TCP is opt-in with authenticated
  # CURVE/ZAP state and is not part of the default desktop profile.

  screenCapture = {
    enable = true;
  };

  prologMcp = {
    enable = true;
  };

  proxmoxMcp = {
    enable = true;
  };

  discordMcp = {
    enable = true;
  };

  unifiMcp = {
    enable = true;
  };

  vibemon = {
    enable = true;
  };

  emacs = {
    enable = true;
    # I mostly use magit hence configured in the ./nixos/mods/emacs.nix module
    gitUser = "N545PY";
    gitEmail = "nsaspy@fedora.email";
    extraPackages = [];
  };
  security.enable = true;
  desktop = {
    # Enable Common sense apps
    enable = true;
    media.enable = true;
    # Setup nerd fonts by default, set desktop.fonts
    fonts.enable = true;

    # TODO Allow module to pass specific folders/paths, for example my ebook dir
    sync.enable = true;
  };
    dev = {
      nim.enable = true;
      common-lisp.enable = true;
      # TODO finish python.enable = true;
    };
  home = {
    username = "unseen";
    homeDirectory = "/home/unseen";
    stateVersion = "23.11";
  };
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update this value in your configuration without breakage.
  # See the Home Manager release notes for a list of state version
  # changes.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

}
