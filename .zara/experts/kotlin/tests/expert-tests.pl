:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_kotlin_expert).
test(identity_and_upstream) :-
    expert_id('zara:expert/kotlin'),
    upstream_contract('lost-rob0t/prolog-rlm#501').
test(kotlin_sources_are_accepted) :- accepts_extension(kt), accepts_extension(kts).
test(java_is_not_silently_accepted, [fail]) :- accepts_extension(java).
test(kotlin_language_features_are_explicit) :-
    supports_semantic(nullability),
    supports_semantic(coroutines),
    supports_semantic(extensions),
    supports_semantic(data_classes),
    supports_semantic(sealed_types).
test(java_records_are_not_kotlin_semantics, [fail]) :- supports_semantic(records).
test(project_metadata_is_observation_only) :-
    project_metadata_role(observation_only),
    android_metadata_role(observation_only).
test(generated_sources_are_excluded) :- generated_source_policy(exclude).
test(current_project_generation_is_accepted) :- generation_current(31, 31).
test(stale_gradle_generation_is_rejected, [fail]) :- generation_current(31, 30).
test(invalid_project_generation_is_rejected, [fail]) :- generation_current(31, unknown).
test(repairs_require_parse_and_compile_verification) :- repair_verification(reparse_and_compile).

test(adapter_applicability_preserves_kotlin_boundary) :-
    language_applicable(
        'src/Main.kt',
        'generation-31',
        language_applicability(
            expert('zara:expert/kotlin'),
            applicable(true),
            extension(kt),
            generation('generation-31')
        )
    ),
    language_applicable(
        'src/Main.java',
        'generation-31',
        language_applicability(
            expert('zara:expert/kotlin'),
            applicable(false),
            extension(java),
            generation('generation-31')
        )
    ).

test(adapter_evidence_keeps_jvm_android_metadata_observation_only) :-
    language_evidence(
        'class Main',
        'generation-31',
        language_evidence(
            expert('zara:expert/kotlin'),
            generation('generation-31'),
            source_chars(10),
            roles([parser, compiler, gradle, jvm_metadata, android_metadata]),
            execution(source_never_executed)
        )
    ).

test(adapter_empty_source_diagnostic_is_deterministic) :-
    language_diagnostic(
        '',
        'generation-31',
        language_diagnostic(
            expert('zara:expert/kotlin'),
            generation('generation-31'),
            diagnostics([diagnostic(error, empty_source)]),
            source_chars(0)
        )
    ).

test(adapter_repair_preview_requires_compile_evidence) :-
    language_repair_preview(
        'class Main',
        'generation-31',
        'diag:kotlin-1',
        language_repair_preview(
            expert('zara:expert/kotlin'),
            status(blocked),
            reason(fresh_evidence_required(parser_and_compile)),
            diagnostic_ref('diag:kotlin-1'),
            generation('generation-31'),
            source_chars(10)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'class Main',
        'data class Main(val x: Int)',
        'generation-32',
        language_repair_verification(
            expert('zara:expert/kotlin'),
            verified(false),
            reason(fresh_postcondition_required(reparse_and_compile)),
            required_postcondition(reparse_and_compile),
            generation('generation-32')
        )
    ).

test(adapter_style_preserves_kotlin_and_metadata_boundaries) :-
    language_style_rules(
        'class Main',
        'style:kotlin-v1',
        language_style(
            expert('zara:expert/kotlin'),
            project_style('style:kotlin-v1'),
            rules([nullability_explicit, coroutine_structure_preserved,
                   extension_resolution_preserved, generated_sources_excluded,
                   gradle_jvm_metadata_observation_only,
                   android_metadata_observation_only]),
            provenance([dotfiles_canonical_brain, project_style('style:kotlin-v1')]),
            source_chars(10)
        )
    ).

test(adapter_explanation_is_provider_free) :-
    language_explanation(
        'decision:kotlin-1',
        'generation-32',
        language_explanation(
            expert('zara:expert/kotlin'),
            decision_ref('decision:kotlin-1'),
            generation('generation-32'),
            basis([dotfiles_canonical_brain,
                   upstream_contract('lost-rob0t/prolog-rlm#501')]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_policy_is_explicit) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).
:- end_tests(dotfiles_kotlin_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
