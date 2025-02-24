# This file collects all modules for easier import in the flake
{ pkgs, config, ... }: {
  imports = [
    ./ai.nix
    ./base.nix
    ./bluetooth.nix
    ./desktop.nix
    ./display.nix
    ./fonts.nix
    ./self-hosted.nix
    ./smb.nix
 ];

  config = {
    nix = {
      package = pkgs.nixVersions.stable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };
    nixpkgs.config.allowUnfree = true;
  };
}