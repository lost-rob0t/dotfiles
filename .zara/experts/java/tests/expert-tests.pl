:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_java_expert).
test(identity_and_upstream) :-
    expert_id('zara:expert/java'),
    upstream_contract('lost-rob0t/prolog-rlm#501').
test(java_is_accepted) :- accepts_extension(java).
test(kotlin_is_not_silently_accepted, [fail]) :- accepts_extension(kt).
test(java_language_features_are_explicit) :-
    supports_semantic(generics),
    supports_semantic(records),
    supports_semantic(modules).
test(kotlin_coroutines_are_not_java_semantics, [fail]) :- supports_semantic(coroutines).
test(project_metadata_is_observation_only) :-
    project_metadata_role(observation_only),
    android_metadata_role(observation_only).
test(generated_sources_are_excluded) :- generated_source_policy(exclude).
test(repairs_require_parse_and_compile_verification) :- repair_verification(reparse_and_compile).
test(zero_model) :- model_calls(0).
:- end_tests(dotfiles_java_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
