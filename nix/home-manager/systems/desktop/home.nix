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
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

}
