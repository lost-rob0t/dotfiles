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
      model_calls/1
    ]).

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
model_calls(0).
