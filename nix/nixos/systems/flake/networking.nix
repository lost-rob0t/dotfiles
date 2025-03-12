{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    22 #ssh
    8384 #syncthing
    22000 # syncthing
    5900 # spice
    { from = 1714; to = 1764; } # Kde connect
  ];
  networking.firewall.allowedUDPPorts = [
    22000 #syncthing
    21027 #syncthing
    5900 # spice
   { from = 1714; to = 1764; } # Kde connect
  ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  ####HOSTS#####
  # TODO implement self-hosted.nix module....
  networking.extraHosts =
    ''
    10.50.50.201 storage.lost.system
    10.50.50.5 dns.lost.system
    10.50.50.30 vpn.lost.system

    '';
  networking.firewall.extraCommands = ''
   iptables -I INPUT 1 -s 172.18.0.0/12 -p tcp -d 172.17.0.1 -j ACCEPT
   iptables -I INPUT 2 -s 172.18.0.0/12 -p udp -d 172.17.0.1 -j ACCEPT
  '';
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.hostName = "flake"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
  };

}
