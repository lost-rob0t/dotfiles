:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_prolog_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/prolog'),
    upstream_contract('lost-rob0t/prolog-rlm#495').

test(language_surface_is_explicit) :-
    accepts_extension(pl),
    accepts_extension(pro),
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

test(adapter_applicability_is_deterministic) :-
    language_applicable(
        'src/main.pl',
        'generation-7',
        language_applicability(
            expert('zara:expert/prolog'),
            applicable(true),
            extension(pl),
            generation('generation-7')
        )
    ),
    language_applicable(
        'src/main.py',
        'generation-7',
        language_applicability(
            expert('zara:expert/prolog'),
            applicable(false),
            extension(py),
            generation('generation-7')
        )
    ).

test(adapter_evidence_never_executes_source) :-
    language_evidence(
        'fact(a).',
        'generation-7',
        language_evidence(
            expert('zara:expert/prolog'),
            generation('generation-7'),
            source_chars(8),
            roles([parser, xref, compiler]),
            execution(source_never_executed)
        )
    ).

test(adapter_diagnostic_requires_fresh_parser_xref) :-
    language_diagnostic(
        'fact(a).',
        'generation-7',
        language_diagnostic(
            expert('zara:expert/prolog'),
            generation('generation-7'),
            diagnostics([diagnostic(pending, fresh_evidence_required(parser_xref))]),
            source_chars(8)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'fact(a).',
        'fact(b).',
        'generation-8',
        language_repair_verification(
            expert('zara:expert/prolog'),
            verified(false),
            reason(fresh_postcondition_required(reparse_and_xref)),
            required_postcondition(reparse_and_xref),
            generation('generation-8')
        )
    ).

test(adapter_style_and_explanation_are_provider_free) :-
    language_style_rules(
        'fact(a).',
        'style:project-v1',
        language_style(
            expert('zara:expert/prolog'),
            project_style('style:project-v1'),
            rules([
                explicit_module_exports,
                predicate_indicators_for_public_api,
                dcg_for_grammar_rules,
                no_unrestricted_call
            ]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v1')]),
            source_chars(8)
        )
    ),
    language_explanation(
        'decision:1',
        'generation-7',
        language_explanation(
            expert('zara:expert/prolog'),
            decision_ref('decision:1'),
            generation('generation-7'),
            basis([
                dotfiles_canonical_brain,
                upstream_contract('lost-rob0t/prolog-rlm#495')
            ]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_prolog_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
