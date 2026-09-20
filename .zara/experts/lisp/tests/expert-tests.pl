:- begin_tests(dotfiles_lisp_expert).

:- use_module('../kb/expert').

test(identity_and_zero_model_budget) :-
    expert_id('zara:expert/lisp'),
    upstream_contract('lost-rob0t/prolog-rlm#494'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(strings_comments_and_reader_chars_do_not_count_as_delimiters) :-
    structural_check(
        '(list "text ) ( \\" still string" ; ) comment\n #| nested ( #| ) |# still ) |# #\\( ?\\) value)',
        structural(ok, balanced)
    ).

test(missing_close_is_localized_and_previewed_as_typed_insert) :-
    Source = '(defun add-one (x) (+ x 1)',
    structural_diagnose(
        Source,
        diagnosis('zara:expert/lisp', structural(error, unmatched_open([0])))
    ),
    preview_repair(
        Source,
        'diagnostic:lisp-missing-close-1',
        repair_preview(
            expert('zara:expert/lisp'),
            status(proposed),
            diagnostic_ref('diagnostic:lisp-missing-close-1'),
            edits([edit(insert, 27, ')')]),
            obligation(fresh_dialect_reader_postcondition)
        )
    ).

test(extra_close_never_auto_deletes) :-
    preview_repair(
        '(list 1 2))',
        'diagnostic:lisp-extra-close-1',
        repair_preview(
            expert('zara:expert/lisp'),
            status(blocked),
            diagnostic_ref('diagnostic:lisp-extra-close-1'),
            reason(unmatched_close_requires_context(10)),
            edits([])
        )
    ).

test(unterminated_reader_state_is_not_misreported_as_paren_balance) :-
    structural_check('(list "oops)', structural(error, unterminated_string)),
    structural_check('#| comment (', structural(error, unterminated_block_comment(1))).

test(generic_balance_is_not_a_dialect_success_postcondition) :-
    verify_repair(
        '(defun add-one (x) (+ x 1)',
        '(defun add-one (x) (+ x 1))',
        repair_verification(
            expert('zara:expert/lisp'),
            verified(false),
            structural_status(balanced),
            reason(fresh_dialect_reader_postcondition_required),
            original_source_preserved(false)
        )
    ).

test(explanation_is_provider_free) :-
    explain_decision(
        'decision:lisp-1',
        explanation(
            expert('zara:expert/lisp'),
            decision_ref('decision:lisp-1'),
            basis([
                reader_aware_structural_scan,
                upstream_contract('lost-rob0t/prolog-rlm#494'),
                canonical_source('lost-rob0t/dotfiles#292')
            ]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

:- end_tests(dotfiles_lisp_expert).

:- initialization(main, main).

main(_) :-
    run_tests,
    halt.
