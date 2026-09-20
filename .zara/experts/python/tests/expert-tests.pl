:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_python_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/python'),
    upstream_contract('lost-rob0t/prolog-rlm#498').

test(language_surface_is_explicit) :-
    accepts_extension(py),
    accepts_extension(pyi),
    supports_semantic(ast_reasoning),
    supports_semantic(scope_reasoning),
    supports_semantic(pattern_matching),
    supports_semantic(indentation).

test(deterministic_tools_are_evidence_only) :-
    evidence_role(ast, observation_only),
    evidence_role(tokenize, observation_only),
    evidence_role(compiler, observation_only),
    source_execution_policy(never).

test(optional_tools_require_configuration) :-
    optional_adapter(type_checker, configured_only),
    optional_adapter(linter, configured_only).

test(generated_sources_are_read_only) :-
    generated_source_policy(read_only).

test(current_generation_is_admitted) :-
    generation_current(11, 11).

test(stale_generation_is_rejected, [fail]) :-
    generation_current(11, 10).

test(repair_requires_fresh_postcondition) :-
    repair_verification(reparse_and_recheck).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_python_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
