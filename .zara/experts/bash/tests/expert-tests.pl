:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_bash_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/bash'),
    upstream_contract('lost-rob0t/prolog-rlm#502').

test(shell_extensions_are_explicit) :-
    accepts_extension(sh),
    accepts_extension(bash).

test(zsh_is_not_silently_accepted, [fail]) :-
    accepts_extension(zsh).

test(startup_files_keep_distinct_semantics) :-
    startup_file('.bash_profile', login_shell),
    startup_file('.profile', login_shell_fallback),
    startup_file('.bashrc', interactive_nonlogin).

test(symbolic_diagnostics_cover_shell_semantics) :-
    supports_semantic(quoting),
    supports_semantic(word_splitting),
    supports_semantic(globbing),
    supports_semantic(arrays),
    supports_semantic(redirection),
    supports_semantic(pipelines),
    supports_semantic(startup_source_graph).

test(read_only_inspection_never_executes_source) :-
    inspection_policy(parse_only),
    source_execution_policy(never).

test(parser_probe_is_read_only) :-
    parser_probe(bash_n, read_only).

test(current_generation_is_accepted) :- generation_current(7, 7).
test(stale_generation_is_rejected, [fail]) :- generation_current(7, 6).
test(invalid_generation_is_rejected, [fail]) :- generation_current(7, stale).

test(repairs_require_fresh_parse_postcondition) :-
    repair_verification(parse_and_bash_n).

test(adapter_applicability_preserves_bash_boundary) :-
    language_applicable(
        'bin/run.sh',
        'generation-7',
        language_applicability(
            expert('zara:expert/bash'),
            applicable(true),
            extension(sh),
            generation('generation-7')
        )
    ),
    language_applicable(
        'bin/run.zsh',
        'generation-7',
        language_applicability(
            expert('zara:expert/bash'),
            applicable(false),
            extension(zsh),
            generation('generation-7')
        )
    ).

test(adapter_evidence_never_executes_source) :-
    language_evidence(
        'echo hi',
        'generation-7',
        language_evidence(
            expert('zara:expert/bash'),
            generation('generation-7'),
            source_chars(7),
            roles([parser, startup_source_graph, quoting, word_splitting, globbing,
                   arrays, redirection, pipelines, functions,
                   command_substitution, process_substitution, heredocs]),
            execution(source_never_executed)
        )
    ).

test(adapter_empty_source_diagnostic_is_deterministic) :-
    language_diagnostic(
        '',
        'generation-7',
        language_diagnostic(
            expert('zara:expert/bash'),
            generation('generation-7'),
            diagnostics([diagnostic(error, empty_source)]),
            source_chars(0)
        )
    ).

test(adapter_repair_preview_requires_fresh_bash_evidence) :-
    language_repair_preview(
        'echo hi',
        'generation-7',
        'diag:1',
        language_repair_preview(
            expert('zara:expert/bash'),
            status(blocked),
            reason(fresh_evidence_required(bash_n_and_shell_semantics)),
            diagnostic_ref('diag:1'),
            generation('generation-7'),
            source_chars(7)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'echo hi',
        'printf hi',
        'generation-8',
        language_repair_verification(
            expert('zara:expert/bash'),
            verified(false),
            reason(fresh_postcondition_required(parse_and_bash_n)),
            required_postcondition(parse_and_bash_n),
            generation('generation-8')
        )
    ).

test(adapter_style_forbids_source_execution) :-
    language_style_rules(
        'echo hi',
        'style:project-v1',
        language_style(
            expert('zara:expert/bash'),
            project_style('style:project-v1'),
            rules([preserve_quoting, preserve_startup_file_semantics,
                   preserve_word_splitting_intent, no_source_execution,
                   parser_evidence_required]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v1')]),
            source_chars(7)
        )
    ).

test(adapter_explanation_is_provider_free) :-
    language_explanation(
        'decision:1',
        'generation-8',
        language_explanation(
            expert('zara:expert/bash'),
            decision_ref('decision:1'),
            generation('generation-8'),
            basis([dotfiles_canonical_brain,
                   upstream_contract('lost-rob0t/prolog-rlm#502')]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_bash_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
