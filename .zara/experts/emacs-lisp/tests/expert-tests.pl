:- begin_tests(dotfiles_emacs_lisp_expert).

:- use_module('../kb/expert').

test(identity_and_zero_model_budget) :-
    expert_id('zara:expert/emacs-lisp'),
    upstream_contract('lost-rob0t/prolog-rlm#497'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(emacs_forms_and_source_features_are_explicit) :-
    defining_form(defun),
    defining_form(defcustom),
    defining_form(defgroup),
    defining_form(define_minor_mode),
    source_feature(lexical_binding_cookie),
    source_feature(autoload_cookie),
    source_feature(byte_compile_evidence),
    source_feature(checkdoc_evidence).

test(structural_reasoning_delegates_to_lisp_expert) :-
    Source = '(defun zara-demo (x) (list x ?\\())',
    structural_check(
        Source,
        emacs_lisp_structural(structural(error, unmatched_open([0])))
    ),
    preview_repair(
        Source,
        'diagnostic:elisp-1',
        delegated_repair(
            from('zara:expert/emacs-lisp'),
            to('zara:expert/lisp'),
            shared_budget(max_model_calls(0)),
            proposal(repair_preview(
                expert('zara:expert/lisp'),
                status(proposed),
                diagnostic_ref('diagnostic:elisp-1'),
                edits([edit(insert, 34, ')')]),
                obligation(fresh_dialect_reader_postcondition)
            )),
            required_postcondition(emacs_fresh_reader_evidence)
        )
    ).

test(generic_balance_never_false_greens_emacs_lisp) :-
    verify_repair(
        '(defun zara-demo (x) (list x ?\\())',
        '(defun zara-demo (x) (list x ?\\()))',
        verification(
            expert('zara:expert/emacs-lisp'),
            verified(false),
            generic(repair_verification(
                expert('zara:expert/lisp'),
                verified(false),
                structural_status(balanced),
                reason(fresh_dialect_reader_postcondition_required),
                original_source_preserved(false)
            )),
            required_postcondition(emacs_fresh_reader_and_byte_compile_evidence)
        )
    ).

test(explanation_names_batch_emacs_evidence) :-
    explain_decision(
        'decision:elisp-1',
        explanation(
            expert('zara:expert/emacs-lisp'),
            decision_ref('decision:elisp-1'),
            basis([
                delegated_structural_engine('zara:expert/lisp'),
                upstream_contract('lost-rob0t/prolog-rlm#497'),
                canonical_source('lost-rob0t/dotfiles#292'),
                dialect_evidence(emacs_batch_reader)
            ]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

:- end_tests(dotfiles_emacs_lisp_expert).

:- initialization(main, main).

main(_) :-
    run_tests,
    halt.
