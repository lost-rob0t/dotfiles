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
      sbcl = super.sbcl.unwrapped.override (old: {
        propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ (with self.pkgs; [
          openssl
          quicklispPackagesClisp.cl-libuv
        ]);
      });
    })


  ];
  # nixpkgs.config.packageOverrides = pkgs: {
  #   nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
  #     inherit pkgs;
  #   };
  # };
  environment.systemPackages = with pkgs; [
    # Utils
    stow
    gcc
    clang
    cmake
    atop
    pandoc #emacs
    hunspellDicts.en_US
    xclip
        cifs-utils
    ## Android
    android-tools
    waydroid
    ## Programming
    pipenv
    direnv
    python313
    pyright
    pylint
    python310Packages.flake8
    nim-unwrapped-2
    nimlsp
    podman-compose
    sqlite

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
        ## Nixos
    nixos-generators

    ## Needed for spice aka virt-man
    spice-vdagent

    # Ricing related.

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
  };
  programs.kdeconnect = {
    enable = true;
  };
 programs.nix-ld.enable = true;

}
