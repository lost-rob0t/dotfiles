{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.agentprolog;
  system = pkgs.stdenv.hostPlatform.system;
  agentprolog = inputs.agentprolog.packages.${system};

  # Main profile: unlimited OpenRouter key (agentprolog-main) resolved at
  # runtime from a private file outside Git; generous operator ceiling for
  # daily driver sessions. Still inert until passed via --config, whether
  # manually or through the agentprolog-main wrapper below.
  mainOperatorConfig = ''
    % AgentProlog main-profile operator configuration (installed by dotfiles).
    % Passed via --config ~/.config/agentprolog/main.pl or the agentprolog-main
    % wrapper. Executable trusted Prolog: review before authorizing.
    :- module(agentprolog_main_operator, [agentprolog_options/1]).

    % Main daily-driver profile backed by the unlimited agentprolog-main
    % OpenRouter key (limit: null). Runtime accounting below stays per-session;
    % the unlimited key has no OpenRouter-side spend cap.
    agentprolog_options([
        trusted_project(true),
        allowed_capabilities([
            tool(workspace_read), tool(workspace_create),
            tool(workspace_write), tool(workspace_modify),
            context(peek)
        ]),
        budget_ceiling(_{
            max_model_calls:200, max_tool_calls:600, max_iterations:128,
            max_total_tokens:1048576, max_output_bytes:2097152,
            max_recursion_depth:2, max_concurrent_subcalls:4,
            max_context_ops:128, time_limit:1800.0, max_cost_usd:25.0
        })
    ]).
  '';

  # Main workspace project: the provider layer for the main profile. The
  # operator file (main.pl) carries trust/caps/budget; the provider lives in
  # the project config per AgentProlog's trust model (operator options never
  # carry runtime_options). Deployed under XDG data home.
  mainWorkspaceConfig = ''
    % AgentProlog main daily-driver workspace: DeepSeek V4.1 Flash via OpenRouter.
    % Credential resolved at runtime from OPENROUTER_API_KEY (agentprolog-main wrapper).
    :- module(agentprolog_main_workspace, [agentprolog_config/1]).

    :- use_module(library(rlm_chain), []).

    agentprolog_config(_{
        profile:main,
        mode:direct,
        runtime_options:[provider(Provider), provider_name(openrouter)]
    }) :-
        rlm_chain:openrouter_provider('deepseek/deepseek-v4.1-flash', Provider).
  '';

  # Wrapper: resolve OPENROUTER_API_KEY from the private key file (never from
  # Nix store or Git), inject the main profile config/state/project defaults,
  # and exec the client. Mirrors the starintel-prolog-rlm-glm credential pattern.
  agentprologMain = pkgs.writeShellApplication {
    name = "agentprolog-main";
    runtimeInputs = [ cfg.package ];
    text = ''
      set -euo pipefail
      keyfile=${lib.escapeShellArg cfg.mainKeyFile}
      if [ -z "''${OPENROUTER_API_KEY:-}" ] && [ -r "$keyfile" ]; then
        mode=$(stat -c %04a "$keyfile")
        if [ "''${mode: -2}" != "00" ]; then
          echo "agentprolog-main: refusing non-private key file: $keyfile" >&2
          exit 78
        fi
        OPENROUTER_API_KEY="$(tr -d '[:space:]' < "$keyfile")"
        export OPENROUTER_API_KEY
      fi
      if [ -z "''${OPENROUTER_API_KEY:-}" ]; then
        echo "agentprolog-main: openrouter credential unavailable; set OPENROUTER_API_KEY or populate $keyfile" >&2
        exit 78
      fi
      main_config=${lib.escapeShellArg "${config.xdg.configHome}/agentprolog/main.pl"}
      main_state=${lib.escapeShellArg "${config.xdg.stateHome}/agentprolog/main"}
      main_project=${lib.escapeShellArg "${config.xdg.dataHome}/agentprolog/main-workspace"}
      args=()
      has_config=0
      has_state=0
      has_project=0
      for arg in "$@"; do
        case "$arg" in
          --config|--config=*) has_config=1 ;;
          --state|--state=*) has_state=1 ;;
          --project|--project=*) has_project=1 ;;
        esac
      done
      if [ "$has_config" -eq 0 ]; then
        args+=("--config" "$main_config")
      fi
      if [ "$has_state" -eq 0 ]; then
        mkdir -p "$main_state"
        args+=("--state" "$main_state")
      fi
      if [ "$has_project" -eq 0 ]; then
        mkdir -p "$main_project"
        args+=("--project" "$main_project")
      fi
      exec agentProlog "''${args[@]}" "$@"
    '';
  };

  # Inert template: AgentProlog never auto-trusts configuration. This file is
  # documentation until the operator passes it explicitly via --config.
  operatorTemplate = ''
    % AgentProlog operator configuration template (installed by dotfiles).
    % NOTHING here takes effect until you pass it explicitly:
    %   agentProlog --project /abs/project --config ~/.config/agentprolog/operator.pl
    % Executable trusted Prolog: review before authorizing. See the wiki:
    %   https://git.starintel.actor/nsaspy/agentProlog/src/branch/main/docs/wiki/Home.org
    :-
      module(agentprolog_operator_template, [agentprolog_options/1]).

    % Minimal read-only starting point. Extend deliberately:
    %  - plugin(File)/package(Root) trust executable Prolog sources
    %  - allowed_capabilities grants native tools (start with nothing)
    %  - budget_ceiling is the operator-only spend ceiling
    agentprolog_options([
        trusted_project(false),
        allowed_capabilities([]),
        budget_ceiling(_{max_model_calls:4, max_tool_calls:8, max_cost_usd:0.02})
    ]).
  '';
in
{
  options.agentprolog = {
    enable = mkEnableOption "AgentProlog canonical conversation CLI and skill importer";

    package = mkOption {
      type = types.package;
      default = agentprolog.agentProlog;
      defaultText = lib.literalExpression "inputs.agentprolog.packages.\${system}.agentProlog";
      description = "AgentProlog terminal client and bundled session host.";
    };

    importSkillPackage = mkOption {
      type = types.package;
      default = agentprolog.import-skill;
      defaultText = lib.literalExpression "inputs.agentprolog.packages.\${system}.import-skill";
      description = "agentprolog-import-skill: convert Markdown skills to inert native Prolog data.";
    };

    zaraRuntimePackage = mkOption {
      type = types.nullOr types.package;
      default = agentprolog.agentProlog-zara-runtime;
      defaultText = lib.literalExpression "inputs.agentprolog.packages.\${system}.agentProlog-zara-runtime";
      description = "Optional Zara runtime profile wrapper (prolog-rlm-zara --profile agentprolog).";
    };

    mainKeyFile = mkOption {
      type = types.str;
      default = "${config.xdg.configHome}/openrouter/agentprolog-main-key";
      description = ''
        Private runtime file holding the unlimited agentprolog-main OpenRouter
        key (mode 0600). Read by the agentprolog-main wrapper only; the secret
        never enters the Nix store or Git.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      [
        cfg.package
        cfg.importSkillPackage
        agentprologMain
      ]
      ++ lib.optional (cfg.zaraRuntimePackage != null) cfg.zaraRuntimePackage;

    xdg.dataFile."agentprolog/main-workspace/.agentprolog/config.pl".text = mainWorkspaceConfig;

    xdg.configFile = {
      "agentprolog/operator.pl".text = operatorTemplate;
      "agentprolog/main.pl".text = mainOperatorConfig;
      "agentprolog/README.org".text = ''
        #+title: AgentProlog global configuration

        AgentProlog has no ambient global configuration: sessions take explicit
        =--project=, =--state= and =--config= inputs, and trusted Prolog is never
        loaded implicitly. =operator.pl= here is an inert template; pass it with
        =--config ~/.config/agentprolog/operator.pl= only after reviewing it.

        The =main= profile for daily-driver sessions uses =main.pl= and the
        =agentprolog-main= wrapper, which resolves the unlimited
        =agentprolog-main= OpenRouter key from
        =~/.config/openrouter/agentprolog-main-key= (0600, never in Git) into
        =OPENROUTER_API_KEY= and defaults =--config=, =--state= and =--project=
        accordingly. The default project (=main-workspace=) supplies the
        DeepSeek V4.1 Flash OpenRouter provider; operator options deliberately
        carry no provider, so pass =--project= to point at another workspace.

        Durable session state defaults to
        =~/.local/state/agentprolog/<sha256-of-project>=; use an explicit
        =--state= directory per agent profile to avoid configuration-binding
        mismatches.

        Full reference: the repository wiki at
        https://git.starintel.actor/nsaspy/agentProlog/src/branch/main/docs/wiki/Home.org
      '';
    };
  };
}
