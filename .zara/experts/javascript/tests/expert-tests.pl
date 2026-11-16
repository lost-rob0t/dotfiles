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
test(current_generation_is_accepted) :- generation_current(7, 7).
test(stale_generation_is_rejected, [fail]) :- generation_current(7, 6).
test(invalid_generation_is_rejected, [fail]) :- generation_current(7, stale).
test(repairs_require_fresh_parse_verification) :- repair_verification(reparse).

test(adapter_applicability_preserves_js_boundary) :-
    language_applicable(
        'src/app.jsx',
        'generation-7',
        language_applicability(
            expert('zara:expert/javascript'),
            applicable(true),
            extension(jsx),
            generation('generation-7')
        )
    ),
    language_applicable(
        'src/app.ts',
        'generation-7',
        language_applicability(
            expert('zara:expert/javascript'),
            applicable(false),
            extension(ts),
            generation('generation-7')
        )
    ).

test(adapter_evidence_is_observation_only) :-
    language_evidence(
        'export const x = 1;',
        'generation-7',
        language_evidence(
            expert('zara:expert/javascript'),
            generation('generation-7'),
            source_chars(19),
            roles([parser, module_resolver, project_metadata]),
            execution(source_never_executed)
        )
    ).

test(adapter_empty_source_diagnostic_is_deterministic) :-
    language_diagnostic(
        '',
        'generation-7',
        language_diagnostic(
            expert('zara:expert/javascript'),
            generation('generation-7'),
            diagnostics([diagnostic(error, empty_source)]),
            source_chars(0)
        )
    ).

test(adapter_repair_preview_requires_fresh_evidence) :-
    language_repair_preview(
        'const x = 1;',
        'generation-7',
        'diag:1',
        language_repair_preview(
            expert('zara:expert/javascript'),
            status(blocked),
            reason(fresh_evidence_required(parser_and_module_resolution)),
            diagnostic_ref('diag:1'),
            generation('generation-7'),
            source_chars(12)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'const x = 1;',
        'const x = 2;',
        'generation-8',
        language_repair_verification(
            expert('zara:expert/javascript'),
            verified(false),
            reason(fresh_postcondition_required(reparse)),
            required_postcondition(reparse),
            generation('generation-8')
        )
    ).

test(adapter_style_keeps_js_distinct_from_ts) :-
    language_style_rules(
        'const x = 1;',
        'style:project-v1',
        language_style(
            expert('zara:expert/javascript'),
            project_style('style:project-v1'),
            rules([preserve_module_system, jsx_only_for_jsx_sources,
                   no_implicit_typescript_semantics,
                   project_metadata_observation_only]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v1')]),
            source_chars(12)
        )
    ).

test(adapter_explanation_is_provider_free) :-
    language_explanation(
        'decision:1',
        'generation-8',
        language_explanation(
            expert('zara:expert/javascript'),
            decision_ref('decision:1'),
            generation('generation-8'),
            basis([dotfiles_canonical_brain,
                   upstream_contract('lost-rob0t/prolog-rlm#500')]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_policy_is_explicit) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).
:- end_tests(dotfiles_javascript_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
