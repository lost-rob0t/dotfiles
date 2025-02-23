# This file collects all modules for easier import in the flake
{ pkgs, config, ... }: {
  imports = [
    ./base.nix
    ./bluetooth.nix
    ./smb.nix
    ./desktop.nix
    ./ai.nix
    ./display.nix
    ./fonts.nix
    ./self-hosted.nix
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