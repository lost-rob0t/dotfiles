# This file collects all modules for easier import in the flake
{ pkgs, config, ... }: {
  imports = [
    ./base.nix
    ./bluetooth.nix
    ./desktop.nix
    ./display.nix
    ./fonts.nix
    ./hibernation.nix
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
