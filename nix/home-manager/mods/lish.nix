{ config, lib, pkgs, ... }:

let
  cfg = config.lish;

  lishBootstrap = pkgs.writeShellApplication {
    name = "lish-bootstrap";
    runtimeInputs = [ pkgs.coreutils pkgs.curl pkgs.git pkgs.sbcl ];
    text = ''
      export STAR_DOTFILES_ROOT=${lib.escapeShellArg cfg.dotfilesRoot}
      ${builtins.readFile ../../../scripts/lish-bootstrap}
    '';
  };

  lishRlm = pkgs.writeShellApplication {
    name = "lish-rlm";
    runtimeInputs = [ pkgs.coreutils pkgs."swi-prolog" ];
    text = ''
      export STAR_DOTFILES_ROOT=${lib.escapeShellArg cfg.dotfilesRoot}
      ${builtins.readFile ../../../scripts/lish-rlm}
    '';
  };

  lishExpert = pkgs.writeShellApplication {
    name = "lish-expert";
    runtimeInputs = [ pkgs.coreutils pkgs.findutils pkgs.gnused pkgs.sbcl pkgs."swi-prolog" ];
    text = ''
      export STAR_DOTFILES_ROOT=${lib.escapeShellArg cfg.dotfilesRoot}
      ${builtins.readFile ../../../scripts/lish-expert}
    '';
  };

  lishShell = pkgs.writeShellApplication {
    name = "lish";
    runtimeInputs = [
      pkgs.bash
      pkgs.coreutils
      pkgs.gnused
      pkgs.sbcl
      lishBootstrap
      lishExpert
      lishRlm
    ];
    text = ''
      export STAR_DOTFILES_ROOT=${lib.escapeShellArg cfg.dotfilesRoot}
      ${builtins.readFile ../../../scripts/lish}
    '';
  };
in
{
  options.lish = {
    enable = lib.mkEnableOption "Lish Common Lisp shell with Prolog-RLM and expert tooling";

    dotfilesRoot = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.dotfiles";
      description = "Dotfiles checkout containing the canonical Lish configuration and expert library.";
    };

    defaultInteractiveShell = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Exec Lish from interactive Bash while retaining Bash as a recovery/login shim.";
    };

    agentic = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Route otherwise-unknown free-form Lish input through Prolog-RLM.";
      };

      mode = lib.mkOption {
        type = lib.types.enum [ "direct" "symbolic" "symbolic-recursive" "auto" ];
        default = "auto";
        description = "Requested Prolog-RLM reasoning strategy for Lish free-form requests.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ lishShell lishBootstrap lishExpert lishRlm pkgs.sbcl pkgs."swi-prolog" ];

    home.sessionVariables = {
      STAR_DOTFILES_ROOT = cfg.dotfilesRoot;
      STAR_LISH_AGENTIC = if cfg.agentic.enable then "1" else "0";
      STAR_LISH_RLM_MODE = cfg.agentic.mode;
    };

    programs.bash.enable = true;
    programs.bash.initExtra = lib.mkIf cfg.defaultInteractiveShell (lib.mkAfter ''
      source ${../../../scripts/lish-bash-enter}
    '');
  };
}
