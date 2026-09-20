:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_nix_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/nix'),
    upstream_contract('lost-rob0t/prolog-rlm#503').

test(nix_extension_is_explicit) :-
    accepts_extension(nix).

test(shell_extension_is_not_silently_accepted, [fail]) :-
    accepts_extension(sh).

test(symbolic_semantics_cover_flakes_modules_and_home_manager) :-
    supports_semantic(parse),
    supports_semantic(flake_reasoning),
    supports_semantic(module_reasoning),
    supports_semantic(home_manager_reasoning),
    supports_semantic(option_provenance).

test(read_only_inspection_never_builds) :-
    inspection_build_policy(never),
    evaluation_policy(explicit_capability),
    build_policy(explicit_capability).

test(parser_probe_is_read_only) :-
    parser_probe(nix_instantiate_parse, read_only).

test(project_metadata_is_observation_only) :-
    project_metadata_role(observation_only).

test(repairs_require_fresh_parse_or_eval_postcondition) :-
    repair_verification(parse_then_eval_or_check).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_nix_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
