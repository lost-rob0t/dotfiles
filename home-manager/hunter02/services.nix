{ config, lib, pkgs, ... }:

{
  services = {
    pantalaimon = {
      enable = false;
    };
    emacs = {
      enable = true;
    };
    syncthing = {
      enable = true;
    };
  };
}
