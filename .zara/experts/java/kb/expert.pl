:- module(dotfiles_java_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      project_metadata_role/1,
      android_metadata_role/1,
      generated_source_policy/1,
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

expert_id('zara:expert/java').
upstream_contract('lost-rob0t/prolog-rlm#501').

accepts_extension(java).

supports_semantic(parse).
supports_semantic(compile_check).
supports_semantic(package_reasoning).
supports_semantic(import_reasoning).
supports_semantic(class_reasoning).
supports_semantic(interface_reasoning).
supports_semantic(generics).
supports_semantic(records).
supports_semantic(modules).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

% Gradle/JVM and Android metadata are evidence, not executable authority.
project_metadata_role(observation_only).
android_metadata_role(observation_only).
generated_source_policy(exclude).

% Gradle/JVM/Android observations are generation-fenced. Only exact current
% metadata may participate in deterministic compile/diagnostic reasoning.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse_and_compile).

% Adapter-ready deterministic operation surface consumed by zara-plugins#874.
% Gradle/JVM/Android values are observations only and can never grant authority.
language_applicable(Path, Generation, Result) :-
    adapter_applicable(
        'zara:expert/java',
        [java],
        Path,
        Generation,
        Result
    ).

language_evidence(Source, Generation, Result) :-
    adapter_evidence(
        'zara:expert/java',
        [parser, compiler, gradle, jvm_metadata, android_metadata],
        Source,
        Generation,
        Result
    ).

language_diagnostic(Source, Generation, Result) :-
    adapter_diagnostic(
        'zara:expert/java',
        parser_and_compile,
        Source,
        Generation,
        Result
    ).

language_repair_preview(Source, Generation, DiagnosticRef, Result) :-
    adapter_repair_preview(
        'zara:expert/java',
        parser_and_compile,
        Source,
        Generation,
        DiagnosticRef,
        Result
    ).

language_repair_verify(OriginalSource, CandidateSource, Generation, Result) :-
    adapter_repair_verify(
        'zara:expert/java',
        reparse_and_compile,
        OriginalSource,
        CandidateSource,
        Generation,
        Result
    ).

language_style_rules(Source, ProjectStyle, Result) :-
    adapter_style_rules(
        'zara:expert/java',
        [ package_import_consistency,
          explicit_record_and_module_semantics,
          generated_sources_excluded,
          gradle_jvm_metadata_observation_only,
          android_metadata_observation_only
        ],
        Source,
        ProjectStyle,
        Result
    ).

language_explanation(DecisionRef, Generation, Result) :-
    adapter_explanation(
        'zara:expert/java',
        'lost-rob0t/prolog-rlm#501',
        DecisionRef,
        Generation,
        Result
    ).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
