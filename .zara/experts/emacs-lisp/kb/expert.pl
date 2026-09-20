:- module(dotfiles_emacs_lisp_expert,
    [ expert_id/1,
      upstream_contract/1,
      can_handle/2,
      structural_check/2,
      structural_diagnose/2,
      preview_repair/3,
      verify_repair/3,
      style_rules/2,
      explain_decision/2,
      defining_form/1,
      source_feature/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

:- use_module('../../lisp/kb/expert', []).

expert_id('zara:expert/emacs-lisp').
upstream_contract('lost-rob0t/prolog-rlm#497').

provider_policy(disabled).
max_model_calls(0).
model_calls(0).

defining_form(defun).
defining_form(defvar).
defining_form(defcustom).
defining_form(defgroup).
defining_form(define_minor_mode).
defining_form(use_package).

source_feature(lexical_binding_cookie).
source_feature(autoload_cookie).
source_feature(quoted_form).
source_feature(backquote_form).
source_feature(byte_compile_evidence).
source_feature(checkdoc_evidence).

can_handle(Path, applicability('zara:expert/emacs-lisp', Applicable)) :-
    atom(Path),
    file_name_extension(_, RawExtension, Path),
    downcase_atom(RawExtension, Extension),
    ( Extension == el -> Applicable = true ; Applicable = false ).

structural_check(Source, emacs_lisp_structural(Generic)) :-
    dotfiles_lisp_expert:structural_check(Source, Generic).

structural_diagnose(Source, diagnosis(
        expert('zara:expert/emacs-lisp'),
        generic(Generic),
        required_evidence([emacs_reader, byte_compile_if_safe, checkdoc_if_configured]))) :-
    dotfiles_lisp_expert:structural_diagnose(Source, Generic).

preview_repair(Source, DiagnosticRef, delegated_repair(
        from('zara:expert/emacs-lisp'),
        to('zara:expert/lisp'),
        shared_budget(max_model_calls(0)),
        proposal(GenericProposal),
        required_postcondition(emacs_fresh_reader_evidence))) :-
    dotfiles_lisp_expert:preview_repair(Source, DiagnosticRef, GenericProposal).

verify_repair(OriginalSource, CandidateSource, verification(
        expert('zara:expert/emacs-lisp'),
        verified(false),
        generic(GenericVerification),
        required_postcondition(emacs_fresh_reader_and_byte_compile_evidence))) :-
    dotfiles_lisp_expert:verify_repair(OriginalSource, CandidateSource, GenericVerification).

style_rules(Source, style(
        expert('zara:expert/emacs-lisp'),
        source_chars(SourceChars),
        rules([
            preserve_lexical_binding_cookie,
            preserve_autoload_cookies,
            prefer_namespaced_symbols,
            docstrings_for_public_forms,
            preserve_reader_structure,
            no_live_editor_state_assumptions
        ]))) :-
    atom(Source),
    atom_length(Source, SourceChars).

explain_decision(DecisionRef, explanation(
        expert('zara:expert/emacs-lisp'),
        decision_ref(DecisionRef),
        basis([
            delegated_structural_engine('zara:expert/lisp'),
            upstream_contract('lost-rob0t/prolog-rlm#497'),
            canonical_source('lost-rob0t/dotfiles#292'),
            dialect_evidence(emacs_batch_reader)
        ]),
        provider_policy(disabled),
        model_calls(0))) :-
    nonvar(DecisionRef).
