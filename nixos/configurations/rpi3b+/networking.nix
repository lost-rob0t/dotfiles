{ config, lib, pkgs, ... }:

{
  networking = {
    # useDHCP = true;
    #interfaces.wlan0 = {
    #  useDHCP = false;
    #  ipv4.addresses = [{
    #    # I used static IP over WLAN because I want to use it as local DNS resolver
    #    address = "10.50.50.101";
    #    prefixLength = 24;
    #  }];
    #};
    interfaces.eth0 = {
      useDHCP = false;
      ipv4.addresses = [{
        address = "10.50.50.100";
        prefixLength = 24;
      }];
    };

    # Enabling WIFI
    wireless.enable = true;
    wireless.interfaces = [ "wlan0" ];
    # If you want to connect also via WIFI to your router
    #wireless.networks."WIFI-SSID".psk = "wifipass";
    # You can set default nameservers
    nameservers = [ "9.9.9.9" "149.112.112.112" "2620:fe::fe"  ];
    # You can set default gateway
    defaultGateway = {
      address = "10.50.50.1";
      interface = "eth0";
    };
  };
}
