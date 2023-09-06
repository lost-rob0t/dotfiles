{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.allowedTCPPorts = [
    22 #ssh
    8384 #syncthing
    22000 # syncthing
  ];
  networking.firewall.allowedUDPPorts = [
    22000 #syncthing
    21027 #syncthing
  ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  ####HOSTS#####
  networking.extraHosts =
    ''
      10.50.50.25 lost-git.local
    '';
  networking.useDHCP = true;
  networking.hostName = "kiosk.local"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
  };

}
