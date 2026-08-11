{
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./disk.nix
    ./hardware-configuration.nix
    ./policy.nix
    ../../mods/hibernation.nix
  ];

  nixpkgs.config.allowUnfree = true;

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      auto-optimise-store = true;
    };

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  boot = {
    kernelModules = lib.mkForce [ ];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "logos";
    networkmanager.enable = true;
    firewall.enable = true;
  };

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";

  hardware = {
    enableRedistributableFirmware = true;
    graphics.enable = true;
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "amdgpu" ];
      xkb.layout = "us";
      displayManager.lightdm.enable = true;
      windowManager.qtile.enable = true;
    };

    # libinput (touchpad/mouse). The enable option moved from
    # services.xserver.libinput to services.libinput in nixpkgs 26.05.
    # Let libinput/Intel defaults handle click behavior, tapping, etc.
    libinput.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    blueman.enable = true;
    dbus.enable = true;
    fstrim.enable = true;
    gvfs.enable = true;
    udisks2.enable = true;
    openssh.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };

  security = {
    polkit.enable = true;
    rtkit.enable = true;
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
  };

  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };

  users.users.unseen = {
    isNormalUser = true;
    description = "unseen";
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
    ];
    shell = pkgs.zsh;
  };

  programs = {
    dconf.enable = true;
    git.enable = true;
    nm-applet.enable = true;
    nix-ld.enable = true;
    zsh.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-qt;
    };
  };

  fileSystems."/home/unseen/share" = {
    device = "//storage.lost.system/unseen";
    fsType = "cifs";
    options = [
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=60"
      "x-systemd.device-timeout=5s"
      "x-systemd.mount-timeout=5s"
      "uid=1000"
      "credentials=/home/unseen/.config/smb-creds"
    ];
  };

  environment.systemPackages = with pkgs; [
    alacritty
    android-tools
    bash
    bluez-tools
    brave
    btop
    cifs-utils
    clang
    cmake
    curl
    docker-compose
    dunst
    emacs
    fd
    feh
    file
    gcc
    git
    git-lfs
    gnumake
    htop
    jq
    libnotify
    networkmanagerapplet
    p7zip
    picom
    pkg-config
    python3
    pyright
    ripgrep
    rofi
    sbcl
    stow
    swi-prolog
    tree
    unzip
    wget
    which
    xclip
  ];

  system.stateVersion = "21.11";
}
