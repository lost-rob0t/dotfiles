{ lib, pkgs, config, ... }:

let
  gitSync = pkgs.writeShellApplication {
    name = "git-sync";
    runtimeInputs = [ pkgs.git ];
    text = builtins.readFile ../../../.config/bash/git-sync.sh;
  };

  gptTodosSync = pkgs.writeShellApplication {
    name = "gpt-todos-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
      pkgs.rsync
      pkgs.util-linux
      pkgs.cronie
    ];
    text = builtins.readFile ../../../scripts/gpt-todos-sync;
  };

  installGptTodosCron = pkgs.writeShellApplication {
    name = "install-gpt-todos-cron";
    runtimeInputs = [ pkgs.cronie ];
    text = builtins.readFile ../../../scripts/install-gpt-todos-cron;
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

    ]) ++ [
      gitSync
      gptTodosSync
      installGptTodosCron
    ];
  };
}
