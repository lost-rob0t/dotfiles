{ config, lib, pkgs, ... }:

# Networking goes into here

{
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [
    22 #ssh
    5984 #couchdb
    8384 #syncthing
    22000 # syncthing
    5332 # caldev
    3128
    15029
    5900 # spice
  ];
  networking.firewall.allowedUDPPorts = [
    22000 #syncthing
    21027 #syncthing
    15029
    5900 # spice
    ];
  networking.firewall.extraCommands = ''iptables -t raw -A OUTPUT -p udp -m udp --dport 137 -j CT --helper netbios-ns'';
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;
  ####HOSTS#####
  #### TODO Create a "global" config for all configurations to import
  #### Incase local DNS ever down (i have crashed it with starintel!)
  networking.extraHosts =
    ''
      10.50.50.5 dns.lost.system
      10.50.50.18 bb.star.intel
      10.50.50.22 gitea.lost.system
      10.50.50.25 music.lost.system
      10.50.50.26 automation.lost.system
      10.50.50.170 health.lost.system
      10.50.50.201 storage.lost.system
      10.50.50.221 bots.star.intel
      10.50.50.222 injest.star.intel
      10.50.50.248 database.star.intel
    '';
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.hostName = "flake"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
  };


}
