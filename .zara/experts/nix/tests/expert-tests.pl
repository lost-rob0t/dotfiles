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

test(current_generation_is_accepted) :- generation_current(7, 7).
test(stale_generation_is_rejected, [fail]) :- generation_current(7, 6).
test(invalid_generation_is_rejected, [fail]) :- generation_current(7, stale).

test(repairs_require_fresh_parse_or_eval_postcondition) :-
    repair_verification(parse_then_eval_or_check).

test(adapter_applicability_preserves_nix_boundary) :-
    language_applicable(
        'flake.nix',
        'generation-7',
        language_applicability(
            expert('zara:expert/nix'),
            applicable(true),
            extension(nix),
            generation('generation-7')
        )
    ),
    language_applicable(
        'deploy.sh',
        'generation-7',
        language_applicability(
            expert('zara:expert/nix'),
            applicable(false),
            extension(sh),
            generation('generation-7')
        )
    ).

test(adapter_evidence_is_observation_only) :-
    language_evidence(
        'let x = 1; in x',
        'generation-7',
        language_evidence(
            expert('zara:expert/nix'),
            generation('generation-7'),
            source_chars(15),
            roles([parser, flake_graph, module_graph, home_manager,
                   option_provenance, derivation_reference, project_metadata]),
            execution(source_never_executed)
        )
    ).

test(adapter_empty_source_diagnostic_is_deterministic) :-
    language_diagnostic(
        '',
        'generation-7',
        language_diagnostic(
            expert('zara:expert/nix'),
            generation('generation-7'),
            diagnostics([diagnostic(error, empty_source)]),
            source_chars(0)
        )
    ).

test(adapter_repair_preview_requires_fresh_nix_evidence) :-
    language_repair_preview(
        '{}',
        'generation-7',
        'diag:1',
        language_repair_preview(
            expert('zara:expert/nix'),
            status(blocked),
            reason(fresh_evidence_required(nix_parse_and_module_evidence)),
            diagnostic_ref('diag:1'),
            generation('generation-7'),
            source_chars(2)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        '{}',
        '{ x = 1; }',
        'generation-8',
        language_repair_verification(
            expert('zara:expert/nix'),
            verified(false),
            reason(fresh_postcondition_required(parse_then_eval_or_check)),
            required_postcondition(parse_then_eval_or_check),
            generation('generation-8')
        )
    ).

test(adapter_style_forbids_implicit_eval_or_build) :-
    language_style_rules(
        '{}',
        'style:project-v1',
        language_style(
            expert('zara:expert/nix'),
            project_style('style:project-v1'),
            rules([preserve_flake_module_structure, preserve_option_provenance,
                   no_implicit_evaluation, no_implicit_build,
                   project_metadata_observation_only]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v1')]),
            source_chars(2)
        )
    ).

test(adapter_explanation_is_provider_free) :-
    language_explanation(
        'decision:1',
        'generation-8',
        language_explanation(
            expert('zara:expert/nix'),
            decision_ref('decision:1'),
            generation('generation-8'),
            basis([dotfiles_canonical_brain,
                   upstream_contract('lost-rob0t/prolog-rlm#503')]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_nix_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
