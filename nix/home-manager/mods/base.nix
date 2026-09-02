{ lib, pkgs, config, ... }:

let
  gitSync = pkgs.writeShellApplication {
    name = "git-sync";
    runtimeInputs = [ pkgs.git ];
    text = builtins.readFile ../../../.config/bash/git-sync.sh;
  };

  dotfilesSync = pkgs.writeShellApplication {
    name = "dotfiles-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.git
      pkgs.openssh
      pkgs.util-linux
      config.emacs.package
    ];
    text = builtins.readFile ../../../scripts/dotfiles-sync;
  };
in
{
  options = {
    base.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable the base module which provides coreutils";
    };
  };

  config = lib.mkIf config.base.enable {
    home.packages = (with pkgs; [
      nixpkgs-fmt
      git
      stow # until i have nix handles it
      grc
      htop
      jq
      fzf
      rar
      zip
      coreutils-full
      progress
      starship
      curl
      tea # Forgejo CLI for git.starintel.actor
      forgejo-cli

    ]) ++ [ gitSync dotfilesSync ];
  };
}
