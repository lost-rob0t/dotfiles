:- module(dotfiles_javascript_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      module_system/1,
      jsx_variant/1,
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

expert_id('zara:expert/javascript').
upstream_contract('lost-rob0t/prolog-rlm#500').

accepts_extension(js).
accepts_extension(jsx).
accepts_extension(mjs).
accepts_extension(cjs).

supports_semantic(parse).
supports_semantic(module_reasoning).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

module_system(esm).
module_system(commonjs).
jsx_variant(jsx).

% Project metadata may refine style/module interpretation, never grant authority.
project_metadata_role(observation_only).

% Syntax trees/project observations are usable only for the exact generation
% requested by the caller. Stale trees fail closed instead of being treated as
% approximately current.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse).

% Adapter-ready deterministic operation surface consumed by zara-plugins#874.
% Source text is inert data: these predicates never execute JavaScript and never
% escalate to TypeScript or a model/provider on ambiguity.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/javascript',
        [js, jsx, mjs, cjs],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/javascript',
        [parser, module_resolver, project_metadata],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/javascript',
        parser_and_module_resolution,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/javascript',
        parser_and_module_resolution,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/javascript',
        reparse,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/javascript',
        [ preserve_module_system,
          jsx_only_for_jsx_sources,
          no_implicit_typescript_semantics,
          project_metadata_observation_only
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/javascript',
        'lost-rob0t/prolog-rlm#500',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
