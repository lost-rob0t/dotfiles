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

test(adapter_applicability_is_deterministic) :-
    language_applicable(
        'pkg/main.py',
        'generation-11',
        language_applicability(
            expert('zara:expert/python'),
            applicable(true),
            extension(py),
            generation('generation-11')
        )
    ),
    language_applicable(
        'pkg/main.nim',
        'generation-11',
        language_applicability(
            expert('zara:expert/python'),
            applicable(false),
            extension(nim),
            generation('generation-11')
        )
    ).

test(adapter_evidence_is_observation_only) :-
    language_evidence(
        'pass',
        'generation-11',
        language_evidence(
            expert('zara:expert/python'),
            generation('generation-11'),
            source_chars(4),
            roles([ast, tokenize, compiler]),
            execution(source_never_executed)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'x=1',
        'x = 1',
        'generation-12',
        language_repair_verification(
            expert('zara:expert/python'),
            verified(false),
            reason(fresh_postcondition_required(reparse_and_recheck)),
            required_postcondition(reparse_and_recheck),
            generation('generation-12')
        )
    ).

test(adapter_style_and_explanation_are_provider_free) :-
    language_style_rules(
        'x = 1',
        'style:project-v2',
        language_style(
            expert('zara:expert/python'),
            project_style('style:project-v2'),
            rules([
                explicit_imports,
                stable_type_hints,
                preserve_indentation,
                no_source_execution
            ]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v2')]),
            source_chars(5)
        )
    ),
    language_explanation(
        'decision:python-1',
        'generation-11',
        language_explanation(
            expert('zara:expert/python'),
            decision_ref('decision:python-1'),
            generation('generation-11'),
            basis([
                dotfiles_canonical_brain,
                upstream_contract('lost-rob0t/prolog-rlm#498')
            ]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_python_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
