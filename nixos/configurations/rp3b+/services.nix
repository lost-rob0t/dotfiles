{ config, lib, pkgs, ... }:

## Services go into here


{
  services = {
    ## Xserver config
    xserver = {
      enable = true;
      displayManager = {
        sddm.enable = true;
        defaultSession = "lxqt";
      };
      desktopManager = {
        lxqt.enable = true;
        mate.enable = true;
      };
      windowManager.qtile.enable = true;
      layout = "us";
    };

    # Enable the OpenSSH daemon.
    openssh = {
      enable = true;
      startWhenNeeded = true;
      permitRootLogin = "yes";
    };
    tor = {
      enable = true;
    };
    printing = {
      enable = false;
    };
    #services.i2p = {
    #  enable = true;
    #};
    emacs = {
      enable = true;
      defaultEditor = true;
    };
    blueman = {
      enable = true;
    };
  };
}
