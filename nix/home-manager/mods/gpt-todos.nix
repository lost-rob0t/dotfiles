{ lib, pkgs, config, ... }:

let
  cfg = config.gptTodos;

  gptTodosSync = pkgs.writeShellApplication {
    name = "gpt-todos-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.git
      pkgs.openssh
      pkgs.rsync
      pkgs.util-linux
      pkgs.cronie
      config.emacs.package
    ];
    text = ''
      export DOTFILES_DIR=/nonexistent/gpt-todos-sync-no-dotfiles
      ${builtins.readFile ../../../scripts/gpt-todos-sync}
    '';
  };

  installGptTodosCron = pkgs.writeShellApplication {
    name = "install-gpt-todos-cron";
    runtimeInputs = [ pkgs.cronie ];
    text = builtins.readFile ../../../scripts/install-gpt-todos-cron;
  };
in
{
  options.gptTodos = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install and run GPT TODO Org synchronization.";
    };

    repoDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/gpt-todos";
      description = "Durable lost-rob0t/gpt-todos checkout.";
    };

    orgDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Documents/Notes/org/agenda";
      description = "Live recursive Org agenda tree synchronized with gpt-todos.";
    };

    syncInterval = lib.mkOption {
      type = lib.types.str;
      default = "5m";
      description = "Interval between background GPT TODO synchronization runs.";
    };
  };

  config = lib.mkIf (config.emacs.enable && cfg.enable) {
    home.packages = [
      gptTodosSync
      installGptTodosCron
    ];

    home.sessionVariables = {
      GPT_TODOS_SYNC = "${gptTodosSync}/bin/gpt-todos-sync";
      GPT_TODOS_REPO_DIR = cfg.repoDir;
      GPT_TODOS_ORG_DIR = cfg.orgDir;
    };

    systemd.user.services.gpt-todos-sync = {
      Unit = {
        Description = "Synchronize durable GPT TODO Org agenda";
        Wants = [ "network-online.target" ];
        After = [ "network-online.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${gptTodosSync}/bin/gpt-todos-sync";
        Environment = [
          "GPT_TODOS_REPO_DIR=${cfg.repoDir}"
          "GPT_TODOS_ORG_DIR=${cfg.orgDir}"
          "DOTFILES_DIR=/nonexistent/gpt-todos-sync-no-dotfiles"
        ];
      };
    };

    systemd.user.timers.gpt-todos-sync = {
      Unit.Description = "Synchronize GPT TODO agenda every ${cfg.syncInterval}";
      Timer = {
        OnBootSec = "2m";
        OnUnitActiveSec = cfg.syncInterval;
        AccuracySec = "15s";
        Unit = "gpt-todos-sync.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
