:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_nim_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/nim'),
    upstream_contract('lost-rob0t/prolog-rlm#499').

test(language_surface_is_explicit) :-
    accepts_extension(nim),
    accepts_extension(nims),
    accepts_extension(nimble),
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

test(adapter_applicability_is_deterministic) :-
    language_applicable(
        'src/main.nim',
        'generation-3',
        language_applicability(
            expert('zara:expert/nim'),
            applicable(true),
            extension(nim),
            generation('generation-3')
        )
    ),
    language_applicable(
        'pkg/tool.nimble',
        'generation-3',
        language_applicability(
            expert('zara:expert/nim'),
            applicable(true),
            extension(nimble),
            generation('generation-3')
        )
    ).

test(adapter_evidence_is_observation_only) :-
    language_evidence(
        'discard',
        'generation-3',
        language_evidence(
            expert('zara:expert/nim'),
            generation('generation-3'),
            source_chars(7),
            roles([parser, compiler]),
            execution(source_never_executed)
        )
    ).

test(adapter_repair_verify_never_false_greens) :-
    language_repair_verify(
        'discard',
        'proc main() = discard',
        'generation-4',
        language_repair_verification(
            expert('zara:expert/nim'),
            verified(false),
            reason(fresh_postcondition_required(reparse_and_nim_check)),
            required_postcondition(reparse_and_nim_check),
            generation('generation-4')
        )
    ).

test(adapter_style_and_explanation_are_provider_free) :-
    language_style_rules(
        'discard',
        'style:project-v3',
        language_style(
            expert('zara:expert/nim'),
            project_style('style:project-v3'),
            rules([
                preserve_significant_indentation,
                explicit_pragmas,
                typed_compiler_argv,
                no_source_execution
            ]),
            provenance([dotfiles_canonical_brain, project_style('style:project-v3')]),
            source_chars(7)
        )
    ),
    language_explanation(
        'decision:nim-1',
        'generation-3',
        language_explanation(
            expert('zara:expert/nim'),
            decision_ref('decision:nim-1'),
            generation('generation-3'),
            basis([
                dotfiles_canonical_brain,
                upstream_contract('lost-rob0t/prolog-rlm#499')
            ]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_nim_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
