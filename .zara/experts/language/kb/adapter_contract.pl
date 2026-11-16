:- module(dotfiles_language_adapter_contract,
    [ adapter_applicable/5,
      adapter_evidence/5,
      adapter_diagnostic/5,
      adapter_repair_preview/6,
      adapter_repair_verify/6,
      adapter_style_rules/5,
      adapter_explanation/5
    ]).

% Shared deterministic ZARA-EXPERT/1 language-brain ABI helpers.
% This module never executes inspected source, never calls a provider, and never
% claims an effect/postcondition succeeded without fresh host evidence.

adapter_applicable(ExpertId, Extensions, Path, Generation, Result) :-
    atom(ExpertId),
    is_list(Extensions),
    atom(Path),
    nonvar(Generation),
    path_extension(Path, Extension),
    ( memberchk(Extension, Extensions) -> Applicable = true ; Applicable = false ),
    Result = language_applicability(
        expert(ExpertId),
        applicable(Applicable),
        extension(Extension),
        generation(Generation)
    ).

adapter_evidence(ExpertId, Roles, Source, Generation, Result) :-
    atom(ExpertId),
    is_list(Roles),
    atom(Source),
    nonvar(Generation),
    atom_length(Source, SourceChars),
    Result = language_evidence(
        expert(ExpertId),
        generation(Generation),
        source_chars(SourceChars),
        roles(Roles),
        execution(source_never_executed)
    ).

adapter_diagnostic(ExpertId, RequiredEvidence, Source, Generation, Result) :-
    atom(ExpertId),
    atom(Source),
    nonvar(Generation),
    atom_length(Source, SourceChars),
    diagnostic_set(SourceChars, RequiredEvidence, Diagnostics),
    Result = language_diagnostic(
        expert(ExpertId),
        generation(Generation),
        diagnostics(Diagnostics),
        source_chars(SourceChars)
    ).

adapter_repair_preview(ExpertId, RequiredEvidence, Source, Generation, DiagnosticRef, Result) :-
    atom(ExpertId),
    atom(Source),
    nonvar(Generation),
    nonvar(DiagnosticRef),
    atom_length(Source, SourceChars),
    Result = language_repair_preview(
        expert(ExpertId),
        status(blocked),
        reason(fresh_evidence_required(RequiredEvidence)),
        diagnostic_ref(DiagnosticRef),
        generation(Generation),
        source_chars(SourceChars)
    ).

adapter_repair_verify(ExpertId, Verification, Original, Candidate, Generation, Result) :-
    atom(ExpertId),
    atom(Original),
    atom(Candidate),
    nonvar(Generation),
    repair_verify_reason(Original, Candidate, Verification, Reason),
    Result = language_repair_verification(
        expert(ExpertId),
        verified(false),
        reason(Reason),
        required_postcondition(Verification),
        generation(Generation)
    ).

adapter_style_rules(ExpertId, Rules, Source, ProjectStyle, Result) :-
    atom(ExpertId),
    is_list(Rules),
    atom(Source),
    nonvar(ProjectStyle),
    atom_length(Source, SourceChars),
    Result = language_style(
        expert(ExpertId),
        project_style(ProjectStyle),
        rules(Rules),
        provenance([dotfiles_canonical_brain, project_style(ProjectStyle)]),
        source_chars(SourceChars)
    ).

adapter_explanation(ExpertId, UpstreamContract, DecisionRef, Generation, Result) :-
    atom(ExpertId),
    nonvar(DecisionRef),
    nonvar(Generation),
    Result = language_explanation(
        expert(ExpertId),
        decision_ref(DecisionRef),
        generation(Generation),
        basis([dotfiles_canonical_brain, upstream_contract(UpstreamContract)]),
        provider_policy(disabled),
        model_calls(0)
    ).

path_extension(Path, Extension) :-
    ( file_name_extension(_, RawExtension, Path) ->
        downcase_atom(RawExtension, Extension)
    ;
        Extension = none
    ).

diagnostic_set(0, _, [diagnostic(error, empty_source)]).
diagnostic_set(SourceChars, RequiredEvidence,
        [diagnostic(pending, fresh_evidence_required(RequiredEvidence))]) :-
    SourceChars > 0.

repair_verify_reason(Original, Candidate, _, no_change) :-
    Original == Candidate,
    !.
repair_verify_reason(_, _, Verification, fresh_postcondition_required(Verification)).
