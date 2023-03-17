{ config, lib, pkgs, ... }:

{
  services = {
    redshift = {
      enabled = true;
    };
  };
}
