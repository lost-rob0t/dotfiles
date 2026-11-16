:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_java_expert).
test(identity_and_upstream) :-
    expert_id('zara:expert/java'),
    upstream_contract('lost-rob0t/prolog-rlm#501').
test(java_is_accepted) :- accepts_extension(java).
test(kotlin_is_not_silently_accepted, [fail]) :- accepts_extension(kt).
test(java_language_features_are_explicit) :-
    supports_semantic(generics),
    supports_semantic(records),
    supports_semantic(modules).
test(kotlin_coroutines_are_not_java_semantics, [fail]) :- supports_semantic(coroutines).
test(project_metadata_is_observation_only) :-
    project_metadata_role(observation_only),
    android_metadata_role(observation_only).
test(generated_sources_are_excluded) :- generated_source_policy(exclude).
test(current_project_generation_is_accepted) :- generation_current(23, 23).
test(stale_gradle_generation_is_rejected, [fail]) :- generation_current(23, 22).
test(invalid_project_generation_is_rejected, [fail]) :- generation_current(23, stale).
test(repairs_require_parse_and_compile_verification) :- repair_verification(reparse_and_compile).

test(adapter_applicability_preserves_java_boundary) :-
    language_applicable(
        'src/Main.java',
        'generation-23',
        language_applicability(
            expert('zara:expert/java'),
            applicable(true),
            extension(java),
            generation('generation-23')
        )
    ),
    language_applicable(
        'src/Main.kt',
        'generation-23',
        language_applicability(
            expert('zara:expert/java'),
            applicable(false),
            extension(kt),
            generation('generation-23')
        )
    ).

test(adapter_evidence_keeps_jvm_android_metadata_observation_only) :-
    language_evidence(
        'class Main {}',
        'generation-23',
        language_evidence(
            expert('zara:expert/java'),
            generation('generation-23'),
            source_chars(13),
            roles([parser, compiler, gradle, jvm_metadata, android_metadata]),
            execution(source_never_executed)
        )
    ).

test(adapter_empty_source_diagnostic_is_deterministic) :-
    language_diagnostic(
        '',
        'generation-23',
        language_diagnostic(
            expert('zara:expert/java'),
            generation('generation-23'),
            diagnostics([diagnostic(error, empty_source)]),
            source_chars(0)
        )
    ).

test(adapter_repair_preview_requires_compile_evidence) :-
    language_repair_preview(
        'class Main {}',
        'generation-23',
        'diag:java-1',
        language_repair_preview(
            expert('zara:expert/java'),
            status(blocked),
            reason(fresh_evidence_required(parser_and_compile)),
            diagnostic_ref('diag:java-1'),
            generation('generation-23'),
            source_chars(13)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'class Main {}',
        'final class Main {}',
        'generation-24',
        language_repair_verification(
            expert('zara:expert/java'),
            verified(false),
            reason(fresh_postcondition_required(reparse_and_compile)),
            required_postcondition(reparse_and_compile),
            generation('generation-24')
        )
    ).

test(adapter_style_preserves_java_and_metadata_boundaries) :-
    language_style_rules(
        'class Main {}',
        'style:jvm-v1',
        language_style(
            expert('zara:expert/java'),
            project_style('style:jvm-v1'),
            rules([package_import_consistency, explicit_record_and_module_semantics,
                   generated_sources_excluded, gradle_jvm_metadata_observation_only,
                   android_metadata_observation_only]),
            provenance([dotfiles_canonical_brain, project_style('style:jvm-v1')]),
            source_chars(13)
        )
    ).

test(adapter_explanation_is_provider_free) :-
    language_explanation(
        'decision:java-1',
        'generation-24',
        language_explanation(
            expert('zara:expert/java'),
            decision_ref('decision:java-1'),
            generation('generation-24'),
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
:- end_tests(dotfiles_java_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
