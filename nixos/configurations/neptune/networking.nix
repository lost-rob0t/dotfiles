{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  ####HOSTS#####
  networking.extraHosts =
  ''
    10.50.50.25 lost-git.local
  '';
  networking.useDHCP = false;
  networking.interfaces.enp7s0.useDHCP = true;
  networking.hostName = "neptune"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    };

}
