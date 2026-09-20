:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_typescript_expert).
test(identity_and_upstream) :-
    expert_id('zara:expert/typescript'),
    upstream_contract('lost-rob0t/prolog-rlm#500').
test(ts_and_tsx_are_accepted) :- accepts_extension(ts), accepts_extension(tsx).
test(js_is_not_silently_accepted, [fail]) :- accepts_extension(js).
test(type_semantics_are_explicit) :-
    supports_semantic(typecheck),
    supports_semantic(type_only_imports),
    supports_semantic(generics).
test(tsx_variant_is_explicit) :- jsx_variant(tsx).
test_config_and_project_metadata_are_observation_only) :-
    compiler_config_role(observation_only),
    project_metadata_role(observation_only).
test(repairs_require_parse_and_typecheck_verification) :- repair_verification(reparse_and_typecheck).
test(zero_model) :- model_calls(0).
:- end_tests(dotfiles_typescript_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
