{ lib, pkgs, config, ... }:

let
  gitSync = pkgs.writeShellApplication {
    name = "git-sync";
    runtimeInputs = [ pkgs.git ];
    text = builtins.readFile ../../../.config/bash/git-sync.sh;
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

    ]) ++ [ gitSync ];

    # Keep the shared helper at the stable path used by interactive Bash and
    # the Qtile sync/reload binding. The installed git-sync command is built
    # from this same file so there is only one implementation to maintain.
    xdg.configFile."bash/git-sync.sh".source = ../../../.config/bash/git-sync.sh;
  };
}
