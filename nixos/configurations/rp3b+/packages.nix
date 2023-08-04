{ config, lib, pkgs, nimPackages, ... }:

## Packages and programs go here
let
  mypkgs = import <personal> { config.allowUnfree = true; };

in
{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };
  environment.systemPackages = with pkgs; [
    # Utils
    wget
    bash
    curl
    git
    stow
    gcc
    clang
    cmake
    ripgrep
    htop
    ispell
    unzip
    bash
    zsh
    p7zip
    coreutils
    hunspellDicts.en_US

    ## User Programs
    firefox
    thunderbird
    mypkgs.tor-browser-bundle-bin
    vim

    yt-dlp
    mypkgs.nimPackages.puffer

    ## Programming
    pipenv
    direnv
    python310
    nim
    nimlsp
    rnix-lsp
    sqlite
    gforth
    ## Security
    keepassxc
    tor
    torsocks
    ## Libs
    libtool
    libvterm
    ((emacsPackagesFor emacsNativeComp).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.ac-ispell
      epkgs.direnv
      epkgs.lsp-pyright
      epkgs.pylint
      epkgs.w3m
      epkgs.pandoc
      mypkgs.nimPackages.puffer
      mypkgs.nimPackages.nimsuggest
      mypkgs.nodePackages.bash-language-server
    ]))

    # Rice
    breeze-icons
  ];
  ## Some programs need SUID wrappers, can be configured further or are
  ## started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
}
