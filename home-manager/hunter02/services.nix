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
      user = "unseen";
      dataDir = "/home/unseen/Documents"; # Default folder for new synced folders
      configDir = "/home/unseen/Documents/.config/syncthing"; # Folder for Syncthing's settings and keys
    };
  };
}
