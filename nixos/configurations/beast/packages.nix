{ config, lib, pkgs, nimPackages, ... }:

## Packages and programs go here
{
   nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
    #(import "/etc/nixos/nixos-overlay/overlay.nix")
    (self: super: {
      python3Packages = super.python3Packages.override {
        overrides = pfinal: pprev: {
          dbus-next = pprev.dbus-next.overridePythonAttrs (old: {
            #  temporary fix for https://github.com/NixOS/nixpkgs/issues/197408
            checkPhase = builtins.replaceStrings ["not test_peer_interface"] ["not test_peer_interface and not test_tcp_connection_with_forwarding"] old.checkPhase;
          });
        };
      };
    })
    (self: super:  {
      qtile = super.qtile.unwrapped.override (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ (with self.python3Packages; [
          requests
          pkgs.sxhkd
          pkgs.j4-dmenu-desktop
          pkgs.dmenu
          pkgs.brave
          pkgs.firefox
          pkgs.emacs
          pkgs.nerdfonts
          pkgs.conky
        ]);
      });
    })
    (self: super: {
      mpv = super.mpv.override {
        scripts = [ self.mpvScripts.mpris ];
      };
    })

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
    atop
    clinfo
    unzip
    bash
    zsh
    p7zip
    coreutils
    pandoc #emacs
    hunspellDicts.en_US
    xclip
    aspell
    aspellDicts.en
    ## Android
    android-tools
    android-studio
    ## User Programs
    ungoogled-chromium
    brave
    thunderbird
    #tor-browser-bundle-bin BUG
    vim
    virt-manager
    virtiofsd
    element-desktop
    qbittorrent
    terminator
    lbry ## You should use lbry and other yotube alternatives
    yt-dlp
    libsForQt5.kdeconnect-kde
    ntfy # send notifications
    mpv
    #mypkgs.nimPackages.puffer
    ## Games
    lutris
    steam-run-native
    wineWowPackages.staging
    winetricks
    faudio
    xboxdrv
    qjoypad
    ##(winetricks.override { wine = wineWowPackages.staging; })

    ## Programming
    pipenv
    direnv
    python310
    pyright
    pylint
    python310Packages.flake8
    nim nimlsp
    podman-compose
    rnix-lsp
    sqlite
    gforth
    racket
    sbcl
    roswell
    ## Security
    keepassxc
    tor
    torsocks
    i2p
    hashcat
    veracrypt
    opensnitch-ui
#    kwalletmanager
    gnome.gnome-keyring
    #mypkgs.maltego
    ## Libs
    libtool
    libvterm
    jdk11

    ((emacsPackagesFor emacsPgtk).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.direnv
      epkgs.lsp-pyright
      epkgs.pylint
      epkgs.w3m
      epkgs.pandoc
      pkgs.nimPackages.nimsuggest
      pkgs.nodePackages.bash-language-server
      pkgs.roswell
      epkgs.xclip
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.libnotify # for alert.el
      pkgs.xdotool # for emacs everywhere
      # Dirvish stuff
      pkgs.ffmpegthumbnailer # Video thumbnails
      pkgs.imagemagick #photo thumbnails
      pkgs.mediainfo #audio previews
      pkgs.mpv # for bongo
      pkgs.pyright
      pkgs.python310
      pkgs.pylint
      pkgs.python310Packages.flake8
    ]))

    # Rice
    breeze-icons
    starship
    variety
    sxhkd
    lxqt.lxqt-qtplugin
    ly #login manager
    xorg.xinit
    j4-dmenu-desktop
    pkgs.dmenu
    conky
    picom
    mpvScripts.mpris
    ## Services
    dunst
    libvirt
    dmenu
    blueman
    ## Nixos
    nixos-generators

    ## Needed for spice
    spice-vdagent
  ];
 hardware.opengl.extraPackages = with pkgs; [
   rocm-opencl-icd
   rocm-opencl-runtime
   ##amdvlk
];
  ## Some programs need SUID wrappers, can be configured further or are
  ## started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = {
   enable = true;
   enableSSHSupport = true;
  };
  programs.adb.enable = true;
  programs.nm-applet.enable = true;


  programs.git = {
    package = pkgs.gitFull;
    enable = true;
  };
  programs.dconf = {
    enable = true;
  };
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };
  programs.kdeconnect = {
   enable = true;
  };
}
