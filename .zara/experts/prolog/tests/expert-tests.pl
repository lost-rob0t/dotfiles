:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_prolog_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/prolog'),
    upstream_contract('lost-rob0t/prolog-rlm#495').

test(language_surface_is_explicit) :-
    accepts_extension(pl),
    accepts_extension(prolog),
    supports_semantic(module_reasoning),
    supports_semantic(predicate_indicator_reasoning),
    supports_semantic(dcg_reasoning),
    supports_semantic(arity_mismatch_diagnostics).

test(source_is_never_executed_as_goal) :-
    source_execution_policy(never),
    unrestricted_call_policy(denied).

test_tooling_is_evidence_only :-
    evidence_role(parser, observation_only),
    evidence_role(xref, observation_only),
    evidence_role(compiler, observation_only).

test(current_generation_is_admitted) :-
    generation_current(7, 7).

test(stale_generation_is_rejected, [fail]) :-
    generation_current(7, 6).

test(invalid_generation_type_is_rejected, [fail]) :-
    generation_current(7, stale).

test(repair_requires_fresh_postcondition) :-
    repair_verification(reparse_and_xref).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_prolog_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
