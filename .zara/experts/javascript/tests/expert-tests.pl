:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_javascript_expert).
test(identity_and_upstream) :-
    expert_id('zara:expert/javascript'),
    upstream_contract('lost-rob0t/prolog-rlm#500').
test(js_and_jsx_are_accepted) :- accepts_extension(js), accepts_extension(jsx).
test(ts_is_not_silently_accepted, [fail]) :- accepts_extension(ts).
test(typecheck_is_not_javascript_semantics, [fail]) :- supports_semantic(typecheck).
test(esm_and_commonjs_are_explicit) :- module_system(esm), module_system(commonjs).
test(project_metadata_is_observation_only) :- project_metadata_role(observation_only).
test(repairs_require_fresh_parse_verification) :- repair_verification(reparse).
test(zero_model) :- model_calls(0).
:- end_tests(dotfiles_javascript_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
