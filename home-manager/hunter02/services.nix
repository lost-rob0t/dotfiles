{ config, lib, pkgs, ... }:

{
  services = {
    emacs = {
      enable = true;
    };
    syncthing = {
      enable = true;
    };
  };
}
