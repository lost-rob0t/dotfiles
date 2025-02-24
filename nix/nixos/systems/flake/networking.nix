{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.enable = false;
  networking.firewall.allowedTCPPorts = [
    22 #ssh
    5984 #couchdb
    8384 #syncthing
    22000 # syncthing
    5332 # caldev
    3128
    15029
    5900 # spice
    47989 # moonlight
    47984 # moonlight
    48010 # moonlight

  ];
  networking.firewall.allowedUDPPorts = [
    22000 #syncthing
    21027 #syncthing
    15029
    5900 # spice
    48010 #moonlight
    47998 # moonlight
    47999 #moonlight
    47800 #moonlight
  ];
  networking.firewall.extraCommands = ''iptables -t raw -A OUTPUT -p udp -m udp --dport 137 -j CT --helper netbios-ns'';
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
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.hostName = "flake"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
  };

}
