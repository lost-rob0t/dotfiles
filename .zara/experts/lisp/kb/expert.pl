:- module(dotfiles_lisp_expert,
    [ expert_id/1,
      upstream_contract/1,
      can_handle/2,
      structural_check/2,
      structural_diagnose/2,
      preview_repair/3,
      verify_repair/3,
      style_rules/2,
      explain_decision/2,
      structural_scan/2,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/lisp').
upstream_contract('lost-rob0t/prolog-rlm#494').

provider_policy(disabled).
max_model_calls(0).
model_calls(0).

can_handle(Path, applicability('zara:expert/lisp', Applicable)) :-
    atom(Path),
    file_name_extension(_, RawExtension, Path),
    downcase_atom(RawExtension, Extension),
    ( memberchk(Extension, [lisp, cl, el, scm]) -> Applicable = true ; Applicable = false ).

structural_check(Source, Result) :-
    structural_scan(Source, Result).

structural_diagnose(Source, diagnosis('zara:expert/lisp', Outcome)) :-
    structural_scan(Source, Outcome).

preview_repair(Source, DiagnosticRef, Result) :-
    atom(Source),
    nonvar(DiagnosticRef),
    structural_scan(Source, Outcome),
    preview_from_outcome(Source, DiagnosticRef, Outcome, Result).

verify_repair(OriginalSource, CandidateSource, Result) :-
    atom(OriginalSource),
    atom(CandidateSource),
    structural_scan(CandidateSource, CandidateOutcome),
    ( CandidateOutcome = structural(ok, balanced) ->
        Result = repair_verification(
            expert('zara:expert/lisp'),
            verified(false),
            structural_status(balanced),
            reason(fresh_dialect_reader_postcondition_required),
            original_source_preserved(false)
        )
    ;
        Result = repair_verification(
            expert('zara:expert/lisp'),
            verified(false),
            structural_status(CandidateOutcome),
            reason(structural_obligation_not_satisfied),
            original_source_preserved(false)
        )
    ).

style_rules(Source, style('zara:expert/lisp', Rules, source_chars(SourceChars))) :-
    atom(Source),
    atom_length(Source, SourceChars),
    Rules = [
        preserve_reader_syntax,
        preserve_symbol_case_policy,
        structural_edits_only,
        no_source_execution,
        fresh_reader_postcondition_required
    ].

explain_decision(DecisionRef, explanation(
        expert('zara:expert/lisp'),
        decision_ref(DecisionRef),
        basis([
            reader_aware_structural_scan,
            upstream_contract('lost-rob0t/prolog-rlm#494'),
            canonical_source('lost-rob0t/dotfiles#292')
        ]),
        provider_policy(disabled),
        model_calls(0))) :-
    nonvar(DecisionRef).

% Reader-aware structural scan. This is a state machine over Lisp reader surface
% syntax, not regex/character counting: strings, escapes, line comments, nested
% #| |# comments, Common Lisp #\\ character literals, and Emacs Lisp ?\\
% character literals are excluded from delimiter accounting.
structural_scan(Source, Outcome) :-
    atom(Source),
    atom_codes(Source, Codes),
    scan_normal(Codes, 0, [], Outcome).

scan_normal([], _, [], structural(ok, balanced)).
scan_normal([], _, Stack, structural(error, unmatched_open(Positions))) :-
    Stack \= [],
    reverse(Stack, Positions).
scan_normal([34|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 1,
    scan_string(Rest, NextPos, Stack, Outcome).
scan_normal([59|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 1,
    scan_line_comment(Rest, NextPos, Stack, Outcome).
scan_normal([35,124|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 2,
    scan_block_comment(Rest, NextPos, Stack, 1, Outcome).
scan_normal([35,92,_Char|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 3,
    scan_normal(Rest, NextPos, Stack, Outcome).
scan_normal([63,92,_Char|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 3,
    scan_normal(Rest, NextPos, Stack, Outcome).
scan_normal([40|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 1,
    scan_normal(Rest, NextPos, [Pos|Stack], Outcome).
scan_normal([41|_], Pos, [], structural(error, unmatched_close(Pos))) :-
    !.
scan_normal([41|Rest], Pos, [_Open|Stack], Outcome) :-
    !,
    NextPos is Pos + 1,
    scan_normal(Rest, NextPos, Stack, Outcome).
scan_normal([_|Rest], Pos, Stack, Outcome) :-
    NextPos is Pos + 1,
    scan_normal(Rest, NextPos, Stack, Outcome).

scan_string([], _, _, structural(error, unterminated_string)).
scan_string([92,_Escaped|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 2,
    scan_string(Rest, NextPos, Stack, Outcome).
scan_string([34|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 1,
    scan_normal(Rest, NextPos, Stack, Outcome).
scan_string([_|Rest], Pos, Stack, Outcome) :-
    NextPos is Pos + 1,
    scan_string(Rest, NextPos, Stack, Outcome).

scan_line_comment([], _, Stack, Outcome) :-
    finish_stack(Stack, Outcome).
scan_line_comment([10|Rest], Pos, Stack, Outcome) :-
    !,
    NextPos is Pos + 1,
    scan_normal(Rest, NextPos, Stack, Outcome).
scan_line_comment([_|Rest], Pos, Stack, Outcome) :-
    NextPos is Pos + 1,
    scan_line_comment(Rest, NextPos, Stack, Outcome).

scan_block_comment([], _, _, Depth,
        structural(error, unterminated_block_comment(Depth))).
scan_block_comment([35,124|Rest], Pos, Stack, Depth, Outcome) :-
    !,
    NextDepth is Depth + 1,
    NextPos is Pos + 2,
    scan_block_comment(Rest, NextPos, Stack, NextDepth, Outcome).
scan_block_comment([124,35|Rest], Pos, Stack, 1, Outcome) :-
    !,
    NextPos is Pos + 2,
    scan_normal(Rest, NextPos, Stack, Outcome).
scan_block_comment([124,35|Rest], Pos, Stack, Depth, Outcome) :-
    Depth > 1,
    !,
    NextDepth is Depth - 1,
    NextPos is Pos + 2,
    scan_block_comment(Rest, NextPos, Stack, NextDepth, Outcome).
scan_block_comment([_|Rest], Pos, Stack, Depth, Outcome) :-
    NextPos is Pos + 1,
    scan_block_comment(Rest, NextPos, Stack, Depth, Outcome).

finish_stack([], structural(ok, balanced)).
finish_stack(Stack, structural(error, unmatched_open(Positions))) :-
    Stack \= [],
    reverse(Stack, Positions).

preview_from_outcome(Source, DiagnosticRef,
        structural(error, unmatched_open(Positions)),
        repair_preview(
            expert('zara:expert/lisp'),
            status(proposed),
            diagnostic_ref(DiagnosticRef),
            edits([edit(insert, EndOffset, ClosingText)]),
            obligation(fresh_dialect_reader_postcondition))) :-
    length(Positions, Count),
    Count > 0,
    atom_length(Source, EndOffset),
    closing_parens(Count, ClosingText).
preview_from_outcome(_, DiagnosticRef,
        structural(error, unmatched_close(Position)),
        repair_preview(
            expert('zara:expert/lisp'),
            status(blocked),
            diagnostic_ref(DiagnosticRef),
            reason(unmatched_close_requires_context(Position)),
            edits([]))).
preview_from_outcome(_, DiagnosticRef,
        structural(error, ReaderFailure),
        repair_preview(
            expert('zara:expert/lisp'),
            status(blocked),
            diagnostic_ref(DiagnosticRef),
            reason(reader_failure(ReaderFailure)),
            edits([]))) :-
    ReaderFailure \= unmatched_close(_),
    ReaderFailure \= unmatched_open(_).
preview_from_outcome(_, DiagnosticRef,
        structural(ok, balanced),
        repair_preview(
            expert('zara:expert/lisp'),
            status(no_change),
            diagnostic_ref(DiagnosticRef),
            edits([]))).

closing_parens(Count, Text) :-
    length(Chars, Count),
    maplist(=(')'), Chars),
    atom_chars(Text, Chars).
