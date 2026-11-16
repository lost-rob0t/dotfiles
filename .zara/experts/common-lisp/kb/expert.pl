:- module(dotfiles_common_lisp_expert,
    [ expert_id/1,
      upstream_contract/1,
      can_handle/2,
      structural_check/2,
      structural_diagnose/2,
      preview_repair/3,
      verify_repair/3,
      style_rules/2,
      explain_decision/2,
      package_operation/1,
      defining_form/1,
      reader_feature/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

:- use_module('../../lisp/kb/expert', []).

expert_id('zara:expert/common-lisp').
upstream_contract('lost-rob0t/prolog-rlm#496').

provider_policy(disabled).
max_model_calls(0).
model_calls(0).

package_operation(in_package).
package_operation(defpackage).
package_operation(import).
package_operation(export).
package_operation(use_package).
package_operation(shadow).

 defining_form(defun).
defining_form(defmacro).
defining_form(defgeneric).
defining_form(defmethod).
defining_form(defclass).
defining_form(defvar).
defining_form(defparameter).

reader_feature(keyword_symbol).
reader_feature(uninterned_symbol).
reader_feature(package_qualified_symbol).
reader_feature(reader_conditional).
reader_feature(dispatch_macro).
reader_feature(block_comment).
reader_feature(character_literal).

can_handle(Path, applicability('zara:expert/common-lisp', Applicable)) :-
    atom(Path),
    file_name_extension(_, RawExtension, Path),
    downcase_atom(RawExtension, Extension),
    ( memberchk(Extension, [lisp, cl, asd]) -> Applicable = true ; Applicable = false ).

structural_check(Source, common_lisp_structural(Generic)) :-
    dotfiles_lisp_expert:structural_check(Source, Generic).

structural_diagnose(Source, diagnosis(
        expert('zara:expert/common-lisp'),
        generic(Generic),
        required_evidence([sbcl_reader, sbcl_compile_if_safe]))) :-
    dotfiles_lisp_expert:structural_diagnose(Source, Generic).

preview_repair(Source, DiagnosticRef, delegated_repair(
        from('zara:expert/common-lisp'),
        to('zara:expert/lisp'),
        shared_budget(max_model_calls(0)),
        proposal(GenericProposal),
        required_postcondition(sbcl_fresh_reader_evidence))) :-
    dotfiles_lisp_expert:preview_repair(Source, DiagnosticRef, GenericProposal).

verify_repair(OriginalSource, CandidateSource, verification(
        expert('zara:expert/common-lisp'),
        verified(false),
        generic(GenericVerification),
        required_postcondition(sbcl_fresh_reader_and_compile_evidence))) :-
    dotfiles_lisp_expert:verify_repair(OriginalSource, CandidateSource, GenericVerification).

style_rules(Source, style(
        expert('zara:expert/common-lisp'),
        source_chars(SourceChars),
        rules([
            explicit_package_context,
            stable_package_qualification,
            declarations_near_binding_forms,
            preserve_reader_conditionals,
            preserve_macro_structure,
            no_source_execution
        ]))) :-
    atom(Source),
    atom_length(Source, SourceChars).

explain_decision(DecisionRef, explanation(
        expert('zara:expert/common-lisp'),
        decision_ref(DecisionRef),
        basis([
            delegated_structural_engine('zara:expert/lisp'),
            upstream_contract('lost-rob0t/prolog-rlm#496'),
            canonical_source('lost-rob0t/dotfiles#292'),
            dialect_evidence(sbcl)
        ]),
        provider_policy(disabled),
        model_calls(0))) :-
    nonvar(DecisionRef).
