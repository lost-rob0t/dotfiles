{config, pkgs, ...}: {
  desktopEnv = {
    enable = true;
    sessionType = "x11";
    qtile = {
      enable = true;
      extraPackages = with pkgs; [
        # Additional packages specific to your setup
        rofi
        dunst
        libnotify
        picom
        feh
        networkmanagerapplet
        volumeicon
      ];
    };
  };

  # Basic X11 configuration
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "";
    
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+qtile";
    };
  };

  # Enable sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
}