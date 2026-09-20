:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_kotlin_expert).
test(identity_and_upstream) :-
    expert_id('zara:expert/kotlin'),
    upstream_contract('lost-rob0t/prolog-rlm#501').
test(kotlin_sources_are_accepted) :- accepts_extension(kt), accepts_extension(kts).
test(java_is_not_silently_accepted, [fail]) :- accepts_extension(java).
test(kotlin_language_features_are_explicit) :-
    supports_semantic(nullability),
    supports_semantic(coroutines),
    supports_semantic(extensions),
    supports_semantic(data_classes),
    supports_semantic(sealed_types).
test(java_records_are_not_kotlin_semantics, [fail]) :- supports_semantic(records).
test(project_metadata_is_observation_only) :-
    project_metadata_role(observation_only),
    android_metadata_role(observation_only).
test(generated_sources_are_excluded) :- generated_source_policy(exclude).
test(repairs_require_parse_and_compile_verification) :- repair_verification(reparse_and_compile).
test(zero_model) :- model_calls(0).
:- end_tests(dotfiles_kotlin_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
