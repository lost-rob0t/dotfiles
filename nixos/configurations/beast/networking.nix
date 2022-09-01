{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.allowedTCPPorts = [ 22 #ssh
                                          5984 #couchdb
                                          8384 #syncthing
                                          22000 # syncthing
                                          5332 # caldev
                                          ];
  networking.firewall.allowedUDPPorts = [ 22000 #syncthing
                                          21027 #syncthing
                                          ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  ####HOSTS#####
  networking.extraHosts =
  ''
    10.50.50.25 lost-git.local
    107.160.74.131 files.catbox.moe # may shutdown soon rip
  '';
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.hostName = "flake"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    };

}
