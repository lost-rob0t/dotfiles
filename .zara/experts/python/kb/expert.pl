:- module(dotfiles_python_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      evidence_role/2,
      optional_adapter/2,
      generated_source_policy/1,
      source_execution_policy/1,
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

expert_id('zara:expert/python').
upstream_contract('lost-rob0t/prolog-rlm#498').

accepts_extension(py).
accepts_extension(pyi).

supports_semantic(parse).
supports_semantic(ast_reasoning).
supports_semantic(scope_reasoning).
supports_semantic(symbol_reasoning).
supports_semantic(import_reasoning).
supports_semantic(type_hint_reasoning).
supports_semantic(decorators).
supports_semantic(async).
supports_semantic(pattern_matching).
supports_semantic(f_strings).
supports_semantic(encoding_comments).
supports_semantic(indentation).
supports_semantic(style).
supports_semantic(repair_verify).

% Deterministic host tooling is evidence, never authority to execute source.
evidence_role(ast, observation_only).
evidence_role(tokenize, observation_only).
evidence_role(compiler, observation_only).
optional_adapter(type_checker, configured_only).
optional_adapter(linter, configured_only).
generated_source_policy(read_only).
source_execution_policy(never).

generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse_and_recheck).

% Adapter-ready deterministic operation surface consumed by zara-plugins#872.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/python',
        [py, pyi],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/python',
        [ast, tokenize, compiler],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/python',
        ast_and_optional_checker,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/python',
        ast_and_optional_checker,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/python',
        reparse_and_recheck,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/python',
        [ explicit_imports,
          stable_type_hints,
          preserve_indentation,
          no_source_execution
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/python',
        'lost-rob0t/prolog-rlm#498',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
