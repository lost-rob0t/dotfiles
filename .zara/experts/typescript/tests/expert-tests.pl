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
test(config_and_project_metadata_are_observation_only) :-
    compiler_config_role(observation_only),
    project_metadata_role(observation_only).
test(current_generation_is_accepted) :- generation_current(12, 12).
test(stale_config_generation_is_rejected, [fail]) :- generation_current(12, 11).
test(invalid_config_generation_is_rejected, [fail]) :- generation_current(12, unknown).
test(repairs_require_parse_and_typecheck_verification) :- repair_verification(reparse_and_typecheck).

test(adapter_applicability_preserves_ts_boundary) :-
    language_applicable(
        'src/app.tsx',
        'generation-12',
        language_applicability(
            expert('zara:expert/typescript'),
            applicable(true),
            extension(tsx),
            generation('generation-12')
        )
    ),
    language_applicable(
        'src/app.js',
        'generation-12',
        language_applicability(
            expert('zara:expert/typescript'),
            applicable(false),
            extension(js),
            generation('generation-12')
        )
    ).

test(adapter_evidence_keeps_config_observation_only) :-
    language_evidence(
        'const x: number = 1;',
        'generation-12',
        language_evidence(
            expert('zara:expert/typescript'),
            generation('generation-12'),
            source_chars(20),
            roles([parser, typechecker, compiler_config, project_metadata]),
            execution(source_never_executed)
        )
    ).

test(adapter_empty_source_diagnostic_is_deterministic) :-
    language_diagnostic(
        '',
        'generation-12',
        language_diagnostic(
            expert('zara:expert/typescript'),
            generation('generation-12'),
            diagnostics([diagnostic(error, empty_source)]),
            source_chars(0)
        )
    ).

test(adapter_repair_preview_requires_typecheck_evidence) :-
    language_repair_preview(
        'const x: number = 1;',
        'generation-12',
        'diag:ts-1',
        language_repair_preview(
            expert('zara:expert/typescript'),
            status(blocked),
            reason(fresh_evidence_required(parser_and_typecheck)),
            diagnostic_ref('diag:ts-1'),
            generation('generation-12'),
            source_chars(20)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'const x: number = 1;',
        'const x: number = 2;',
        'generation-13',
        language_repair_verification(
            expert('zara:expert/typescript'),
            verified(false),
            reason(fresh_postcondition_required(reparse_and_typecheck)),
            required_postcondition(reparse_and_typecheck),
            generation('generation-13')
        )
    ).

test(adapter_style_keeps_ts_compiler_boundary) :-
    language_style_rules(
        'const x: number = 1;',
        'style:project-v2',
        language_style(
            expert('zara:expert/typescript'),
            project_style('style:project-v2'),
            rules([preserve_module_system, explicit_type_only_imports,
                   typecheck_after_repair, compiler_config_observation_only,
                   project_metadata_observation_only]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v2')]),
            source_chars(20)
        )
    ).

test(adapter_explanation_is_provider_free) :-
    language_explanation(
        'decision:ts-1',
        'generation-13',
        language_explanation(
            expert('zara:expert/typescript'),
            decision_ref('decision:ts-1'),
            generation('generation-13'),
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
:- end_tests(dotfiles_typescript_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
