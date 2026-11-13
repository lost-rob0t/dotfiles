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
  };

  config = mkIf cfg.enable {
    home.packages =
      [
        cfg.package
        cfg.importSkillPackage
      ]
      ++ lib.optional (cfg.zaraRuntimePackage != null) cfg.zaraRuntimePackage;

    xdg.configFile = {
      "agentprolog/operator.pl".text = operatorTemplate;
      "agentprolog/README.org".text = ''
        #+title: AgentProlog global configuration

        AgentProlog has no ambient global configuration: sessions take explicit
        =--project=, =--state= and =--config= inputs, and trusted Prolog is never
        loaded implicitly. =operator.pl= here is an inert template; pass it with
        =--config ~/.config/agentprolog/operator.pl= only after reviewing it.

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
