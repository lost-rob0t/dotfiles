{ config, lib, pkgs, ... }:

{
  users = {
    defaultUserShell = pkgs.bash;
    users = {

    unseen = {
      extraGroups = [ "wheel" ];
      initialPassword = "password";
      group = "unseen";
      isNormalUser = true;
    };
    };
  };
}
