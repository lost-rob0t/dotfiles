:- module(dotfiles_kotlin_expert,
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

expert_id('zara:expert/kotlin').
upstream_contract('lost-rob0t/prolog-rlm#501').

accepts_extension(kt).
accepts_extension(kts).

supports_semantic(parse).
supports_semantic(compile_check).
supports_semantic(package_reasoning).
supports_semantic(import_reasoning).
supports_semantic(class_reasoning).
supports_semantic(object_reasoning).
supports_semantic(function_reasoning).
supports_semantic(nullability).
supports_semantic(coroutines).
supports_semantic(extensions).
supports_semantic(data_classes).
supports_semantic(sealed_types).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

% Gradle/JVM and Android metadata are evidence, not executable authority.
project_metadata_role(observation_only).
android_metadata_role(observation_only).
generated_source_policy(exclude).

% Gradle/JVM/Android observations are generation-fenced. Only exact current
% metadata may participate in deterministic compile/coroutine reasoning.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse_and_compile).
model_calls(0).
