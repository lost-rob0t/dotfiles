:- module(dotfiles_nim_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      evidence_role/2,
      compiler_probe/2,
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

expert_id('zara:expert/nim').
upstream_contract('lost-rob0t/prolog-rlm#499').

accepts_extension(nim).
accepts_extension(nims).
accepts_extension(nimble).

supports_semantic(parse).
supports_semantic(compile_check).
supports_semantic(indentation).
supports_semantic(proc_reasoning).
supports_semantic(func_reasoning).
supports_semantic(template_reasoning).
supports_semantic(macro_reasoning).
supports_semantic(type_reasoning).
supports_semantic(import_reasoning).
supports_semantic(generics).
supports_semantic(pragmas).
supports_semantic(style).
supports_semantic(repair_verify).

% Compiler diagnostics are typed observations; source is never executed by inspection.
evidence_role(parser, observation_only).
evidence_role(compiler, observation_only).
compiler_probe(nim_check, typed_argv).
source_execution_policy(never).

generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse_and_nim_check).

% Adapter-ready deterministic operation surface consumed by zara-plugins#872.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/nim',
        [nim, nims, nimble],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/nim',
        [parser, compiler],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/nim',
        parser_and_nim_check,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/nim',
        parser_and_nim_check,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/nim',
        reparse_and_nim_check,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/nim',
        [ preserve_significant_indentation,
          explicit_pragmas,
          typed_compiler_argv,
          no_source_execution
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/nim',
        'lost-rob0t/prolog-rlm#499',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
