:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_nim_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/nim'),
    upstream_contract('lost-rob0t/prolog-rlm#499').

test(language_surface_is_explicit) :-
    accepts_extension(nim),
    accepts_extension(nims),
    supports_semantic(proc_reasoning),
    supports_semantic(template_reasoning),
    supports_semantic(macro_reasoning),
    supports_semantic(generics),
    supports_semantic(pragmas).

test(compiler_evidence_is_typed_and_read_only) :-
    evidence_role(parser, observation_only),
    evidence_role(compiler, observation_only),
    compiler_probe(nim_check, typed_argv),
    source_execution_policy(never).

test(current_generation_is_admitted) :-
    generation_current(3, 3).

test(stale_generation_is_rejected, [fail]) :-
    generation_current(3, 2).

test(repair_requires_fresh_postcondition) :-
    repair_verification(reparse_and_nim_check).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_nim_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
