{ config, lib, pkgs, ... }:

{
  systemd.user.startServices = true;
  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
  };
}
