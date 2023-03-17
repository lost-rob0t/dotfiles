{ config, lib, pkgs, ... }:

{
  services = {
    redshift = {
      enable = true;
      provider = "geoclue2";
    };
  };
}
