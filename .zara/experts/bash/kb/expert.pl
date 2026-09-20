:- module(dotfiles_bash_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      startup_file/2,
      inspection_policy/1,
      source_execution_policy/1,
      parser_probe/2,
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

expert_id('zara:expert/bash').
upstream_contract('lost-rob0t/prolog-rlm#502').

accepts_extension(sh).
accepts_extension(bash).

supports_semantic(parse).
supports_semantic(diagnose).
supports_semantic(quoting).
supports_semantic(word_splitting).
supports_semantic(globbing).
supports_semantic(arrays).
supports_semantic(redirection).
supports_semantic(pipelines).
supports_semantic(functions).
supports_semantic(command_substitution).
supports_semantic(process_substitution).
supports_semantic(heredocs).
supports_semantic(startup_source_graph).
supports_semantic(style).
supports_semantic(repair_verify).

startup_file('.bash_profile', login_shell).
startup_file('.profile', login_shell_fallback).
startup_file('.bashrc', interactive_nonlogin).

% Read-only inspection parses inert source data. It never sources or executes it.
inspection_policy(parse_only).
source_execution_policy(never).
parser_probe(bash_n, read_only).

% Source/startup observations are usable only for the exact generation requested
% by the caller. Stale observations fail closed.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

% A repair cannot be called successful until fresh parser evidence exists.
repair_verification(parse_and_bash_n).

% Adapter-ready deterministic operation surface consumed by zara-plugins.
% Source is always inert data: these predicates never source or execute it and
% never fall back to a provider/model when shell semantics are ambiguous.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/bash',
        [sh, bash],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/bash',
        [parser, startup_source_graph, quoting, word_splitting, globbing,
         arrays, redirection, pipelines, functions, command_substitution,
         process_substitution, heredocs],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/bash',
        bash_n_and_shell_semantics,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/bash',
        bash_n_and_shell_semantics,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/bash',
        parse_and_bash_n,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/bash',
        [ preserve_quoting,
          preserve_startup_file_semantics,
          preserve_word_splitting_intent,
          no_source_execution,
          parser_evidence_required
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/bash',
        'lost-rob0t/prolog-rlm#502',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
