:- module(dotfiles_prolog_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      evidence_role/2,
      source_execution_policy/1,
      unrestricted_call_policy/1,
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

expert_id('zara:expert/prolog').
upstream_contract('lost-rob0t/prolog-rlm#495').

accepts_extension(pl).
accepts_extension(pro).
accepts_extension(prolog).

supports_semantic(parse).
supports_semantic(module_reasoning).
supports_semantic(predicate_indicator_reasoning).
supports_semantic(import_export_reasoning).
supports_semantic(dcg_reasoning).
supports_semantic(singleton_diagnostics).
supports_semantic(undefined_predicate_diagnostics).
supports_semantic(arity_mismatch_diagnostics).
supports_semantic(style).
supports_semantic(repair_verify).

% Compiler/xref observations are evidence only; source text never becomes a goal.
evidence_role(parser, observation_only).
evidence_role(xref, observation_only).
evidence_role(compiler, observation_only).
source_execution_policy(never).
unrestricted_call_policy(denied).

% Any cached parser/xref/compiler evidence must match the current workspace generation.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

% Repair success requires fresh parser/xref evidence after the candidate edit.
repair_verification(reparse_and_xref).

% Adapter-ready deterministic operation surface consumed by zara-plugins#872.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/prolog',
        [pl, pro, prolog],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/prolog',
        [parser, xref, compiler],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/prolog',
        parser_xref,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/prolog',
        parser_xref,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/prolog',
        reparse_and_xref,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/prolog',
        [ explicit_module_exports,
          predicate_indicators_for_public_api,
          dcg_for_grammar_rules,
          no_unrestricted_call
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/prolog',
        'lost-rob0t/prolog-rlm#495',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
