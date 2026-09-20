:- module(dotfiles_typescript_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      module_system/1,
      jsx_variant/1,
      compiler_config_role/1,
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

expert_id('zara:expert/typescript').
upstream_contract('lost-rob0t/prolog-rlm#500').

accepts_extension(ts).
accepts_extension(tsx).
accepts_extension(mts).
accepts_extension(cts).

supports_semantic(parse).
supports_semantic(module_reasoning).
supports_semantic(typecheck).
supports_semantic(type_only_imports).
supports_semantic(generics).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

module_system(esm).
module_system(commonjs).
jsx_variant(tsx).

% tsconfig/project facts refine deterministic compiler semantics, never authority.
compiler_config_role(observation_only).
project_metadata_role(observation_only).

% Parsed trees, compiler-config observations and project metadata must match the
% exact requested generation; stale config must not leak into a later turn.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse_and_typecheck).

% Adapter-ready deterministic operation surface consumed by zara-plugins#874.
% Source text and tsconfig/project observations stay inert; ambiguity never
% falls back to JavaScript semantics or a provider/model.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/typescript',
        [ts, tsx, mts, cts],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/typescript',
        [parser, typechecker, compiler_config, project_metadata],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/typescript',
        parser_and_typecheck,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/typescript',
        parser_and_typecheck,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/typescript',
        reparse_and_typecheck,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/typescript',
        [ preserve_module_system,
          explicit_type_only_imports,
          typecheck_after_repair,
          compiler_config_observation_only,
          project_metadata_observation_only
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/typescript',
        'lost-rob0t/prolog-rlm#500',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
