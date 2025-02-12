{ config, lib, pkgs, nimPackages, inputs, ... }:

## Packages and programs go here
{
  nixpkgs.overlays = [
    #(import "/etc/nixos/nixos-overlay/overlay.nix")
    (self: super: {
      python3Packages = super.python3Packages.override {
        overrides = pfinal: pprev: {
          dbus-next = pprev.dbus-next.overridePythonAttrs (old: {
            #  temporary fix for https://github.com/NixOS/nixpkgs/issues/197408
            checkPhase = builtins.replaceStrings [ "not test_peer_interface" ] [ "not test_peer_interface and not test_tcp_connection_with_forwarding" ] old.checkPhase;
          });
        };
      };
    })
    (self: super: {
      qtile = super.qtile.unwrapped.override (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ (with self.python3Packages; [
          requests
          pika
          pkgs.sxhkd
          pkgs.j4-dmenu-desktop
          pkgs.dmenu
          pkgs.brave
          pkgs.firefox
          pkgs.emacs
          pkgs.nerdfonts
          pkgs.conky
          pkgs.emojione # wttr widget emojis
          pkgs.noto-fonts-emoji
        ]);
      });
    })

    (self: super: {
      sbcl = super.sbcl.unwrapped.override (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ (with self.pkgs; [
          openssl
          quicklispPackagesClisp.cl-libuv
        ]);
      });
    })

    (self: super: {
      mpv = super.mpv.override {
        scripts = [ self.mpvScripts.thumbnail
                    self.mpvScripts.sponsorblock
                    self.mpvScripts.mpv-notify-send
                    self.mpvScripts.videoclip
                    self.mpvScripts.mpv-webm
                    self.mpvScripts.memo
                    self.mpvScripts.modernx
                    self.mpvScripts.autocrop
                    self.mpvScripts.quality-menu
                    self.mpvScripts.mpris

                  ];
      };
    })

  ];
  # nixpkgs.config.packageOverrides = pkgs: {
  #   nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
  #     inherit pkgs;
  #   };
  # };
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
    unzip
    bash
    zsh
    p7zip
    coreutils
    pandoc #emacs
    hunspellDicts.en_US
    xclip
    aspell
    (aspellWithDicts
        (dicts: with dicts; [ en en-computers en-science  ]))
    cifs-utils
    pmutils # sleep/hibernate
    ## Android
    android-tools
    waydroid
    ## Games
    lutris
    steam-run-native
    winePackages.stagingFull
    wineWowPackages.staging
    winetricks
    faudio
    qjoypad
    sunshine
    ##(winetricks.override { wine = wineWowPackages.staging; })

    ## Programming
    pipenv
    direnv
    python3
    pyright
    pylint
    python3Packages.flake8
    nim-unwrapped-2
    nimlsp
    podman-compose
    sqlite
    (python3.withPackages (ps: with ps; [ requests ]))

    ## Security
    tor-browser-bundle-bin
    tor
    torsocks
    i2p
    hashcat
    opensnitch-ui
    calyx-vpn
    gnome-keyring

    # lxqt
    lxqt.lxqt-policykit
    #mypkgs.maltego
    ## Libs
    libtool
    libvterm
    jdk11
    # Rice
    breeze-icons
    lxqt.lxqt-qtplugin
    ly #login manager
    xorg.xinit
    picom
    mpvScripts.mpris
    ## Services
    dunst
    mimic # TTS
    libvirt
    dmenu
    #inputs.nixpkgs-stable.legacyPackages.x86_64-linux.blueman
    blueman
    bluez-tools
    ## Nixos
    nixos-generators

    ## Needed for spice aka virt-man
    spice-vdagent

    # Ricing related.
    xsettingsd


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
