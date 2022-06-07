{ config, lib, pkgs, ... }:
let
  name = "N545PY";
  email = "nsaspy@airmail.cc";
  
in
{
  imports = [ ./programs.nix ./services.nix ];
  home.username = "nsaspy";
  home.homeDirectory = "/home/nsaspy";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs = {
      git = {
        enable = true;
        userName = "${name}";
        userEmail = "${email}";
      };
  emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
};
  };

}
