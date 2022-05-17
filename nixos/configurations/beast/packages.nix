{ config, lib, pkgs, ... }:

## Packages and programs go here

{
   nixpkgs.config.allowUnfree = true;
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
  services.emacs.package = pkgs.emacsUnstable;
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
    atop
    clinfo
    ispell
    unzip
    bash
    zsh
    p7zip
    coreutils
    pandoc #emacs
    hunspellDicts.en_US

    vim
    yt-dlp

    ## Programming
    pipenv
    direnv
    python310
    nim nimlsp
    podman-compose
    rnix-lsp
    sqlite
    ## Security
    keepassxc
    tor
    torsocks
    hashcat
    veracrypt
    ## Libs
    libtool
    libvterm
    jdk11
    ((emacsPackagesNgGen emacsNativeComp).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.ac-ispell
      epkgs.direnv
      epkgs.lsp-pyright
      epkgs.pylint
      epkgs.w3m
      epkgs.pandoc
    ]))

    # Rice
    breeze-icons
    starship
    variety
    sxhkd
    ## Services
    dunst
    libvirt
    dmenu
    ## Nixos
    nixos-generators
  ];
 hardware.opengl.extraPackages = with pkgs; [
   rocm-opencl-icd
   rocm-opencl-runtime
   amdvlk
];
  ## Some programs need SUID wrappers, can be configured further or are
  ## started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = {
   enable = true;
   enableSSHSupport = true;
  };
  programs.nm-applet.enable = true;


  programs.git = {
    package = pkgs.gitFull;
    enable = true;
  };
  programs.dconf = {
    enable = true;
  };
}