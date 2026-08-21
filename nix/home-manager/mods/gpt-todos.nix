{ lib, pkgs, config, ... }:

let
  gptTodosSync = pkgs.writeShellApplication {
    name = "gpt-todos-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
      pkgs.rsync
      pkgs.util-linux
      pkgs.cronie
      config.emacs.package
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
  options.gptTodos.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Install GPT TODO Org synchronization commands.";
  };

  config = lib.mkIf (config.emacs.enable && config.gptTodos.enable) {
    home.packages = [
      gptTodosSync
      installGptTodosCron
    ];
  };
}
