:- begin_tests(dotfiles_common_lisp_expert).

:- use_module('../kb/expert').

test(identity_and_zero_model_budget) :-
    expert_id('zara:expert/common-lisp'),
    upstream_contract('lost-rob0t/prolog-rlm#496'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(package_and_reader_semantics_are_explicit) :-
    package_operation(defpackage),
    package_operation(in_package),
    package_operation(export),
    defining_form(defmacro),
    defining_form(defgeneric),
    defining_form(defmethod),
    reader_feature(keyword_symbol),
    reader_feature(uninterned_symbol),
    reader_feature(reader_conditional),
    reader_feature(dispatch_macro).

test(structural_reasoning_delegates_to_lisp_expert) :-
    structural_check(
        '(defun greet (name) (format nil "hi ~a" name)',
        common_lisp_structural(structural(error, unmatched_open([0])))
    ),
    preview_repair(
        '(defun greet (name) (format nil "hi ~a" name)',
        'diagnostic:cl-1',
        delegated_repair(
            from('zara:expert/common-lisp'),
            to('zara:expert/lisp'),
            shared_budget(max_model_calls(0)),
            proposal(repair_preview(
                expert('zara:expert/lisp'),
                status(proposed),
                diagnostic_ref('diagnostic:cl-1'),
                edits([edit(insert, 45, ')')]),
                obligation(fresh_dialect_reader_postcondition)
            )),
            required_postcondition(sbcl_fresh_reader_evidence)
        )
    ).

test(generic_balance_never_false_greens_common_lisp) :-
    verify_repair(
        '(defun greet (name) (format nil "hi ~a" name)',
        '(defun greet (name) (format nil "hi ~a" name))',
        verification(
            expert('zara:expert/common-lisp'),
            verified(false),
            generic(repair_verification(
                expert('zara:expert/lisp'),
                verified(false),
                structural_status(balanced),
                reason(fresh_dialect_reader_postcondition_required),
                original_source_preserved(false)
            )),
            required_postcondition(sbcl_fresh_reader_and_compile_evidence)
        )
    ).

test(explanation_names_real_dialect_evidence) :-
    explain_decision(
        'decision:cl-1',
        explanation(
            expert('zara:expert/common-lisp'),
            decision_ref('decision:cl-1'),
            basis([
                delegated_structural_engine('zara:expert/lisp'),
                upstream_contract('lost-rob0t/prolog-rlm#496'),
                canonical_source('lost-rob0t/dotfiles#292'),
                dialect_evidence(sbcl)
            ]),
            provider_policy(disabled),
            model_calls(0)
        )
    ).

:- end_tests(dotfiles_common_lisp_expert).

:- initialization(main, main).

main(_) :-
    run_tests,
    halt.
