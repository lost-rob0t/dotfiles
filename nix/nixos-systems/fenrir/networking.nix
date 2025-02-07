{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.allowedTCPPorts = [
    22 #ssh
    5984 #couchdb
    8384 #syncthing
    22000 # syncthing
    5332 # caldev
    8080 # testing
    15029

    47989 # moonlight
    47984 # moonlight
    48010 # moonlight
  ];
  networking.firewall.allowedUDPPorts = [
    22000 #syncthing
    21027 #syncthing
    15029
    48010 #moonlight
    47998 # moonlight
    47999 #moonlight
    47800 #moonlight
  ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  ####HOSTS#####
  networking.extraHosts =
    ''
      10.50.50.5 search.goyim.fre
      10.50.50.25 lost-git.local
    '';
  networking.useDHCP = false;
  #networking.interfaces.eno1.useDHCP = true;
  networking.hostName = "fenrir"; # Lokis wolf
  networking.networkmanager = {
    enable = true;
  };

}
