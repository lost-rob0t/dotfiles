:- module(dotfiles_nix_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      inspection_build_policy/1,
      evaluation_policy/1,
      build_policy/1,
      parser_probe/2,
      project_metadata_role/1,
      generation_current/2,
      repair_verification/1,
      language_applicable/3,
      language_evidence/3,
      language_diagnostic/3,
      language_repair_preview/4,
      language_repair_verify/4,
      language_style_rules/3,
      language_explanation/3,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

:- use_module('../../language/kb/adapter_contract').

expert_id('zara:expert/nix').
upstream_contract('lost-rob0t/prolog-rlm#503').

accepts_extension(nix).

supports_semantic(parse).
supports_semantic(flake_reasoning).
supports_semantic(module_reasoning).
supports_semantic(home_manager_reasoning).
supports_semantic(option_provenance).
supports_semantic(derivation_reference).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

% Parsing and ownership inspection must never trigger evaluation or a build.
inspection_build_policy(never).
evaluation_policy(explicit_capability).
build_policy(explicit_capability).
parser_probe(nix_instantiate_parse, read_only).
project_metadata_role(observation_only).

% Source/project observations are usable only for the exact generation requested
% by the caller. Stale observations fail closed instead of being approximately
% accepted by the adapter.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

% Repairs require fresh parse evidence and, when the operation used evaluation or
% module checking, a fresh bounded check through the host-owned capability path.
repair_verification(parse_then_eval_or_check).

% Adapter-ready deterministic operation surface consumed by zara-plugins.
% Source text is inert data. These predicates never evaluate or build Nix and do
% not call a provider on ambiguity or missing evidence.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/nix',
        [nix],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/nix',
        [parser, flake_graph, module_graph, home_manager,
         option_provenance, derivation_reference, project_metadata],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/nix',
        nix_parse_and_module_evidence,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/nix',
        nix_parse_and_module_evidence,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/nix',
        parse_then_eval_or_check,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/nix',
        [ preserve_flake_module_structure,
          preserve_option_provenance,
          no_implicit_evaluation,
          no_implicit_build,
          project_metadata_observation_only
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/nix',
        'lost-rob0t/prolog-rlm#503',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
