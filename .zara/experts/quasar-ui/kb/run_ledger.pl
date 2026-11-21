%% Run-ledger reader for the quasar-ui expert.
%%
%% Consumes the quasar checkout's run ledger (.prolog/runs/run-<HEAD>.pl,
%% written by quasar scripts/record-run.mjs) through an explicit-path
%% adapter: load the directory, then reason over per-commit suite results.
%% This is the self-updating half of the KB - every recorded run extends
%% it; nothing here writes.
%%
%% Facts loaded:
%%   run_fact(Commit, Seq, SuiteTerm, Result, Passed, Failed, When)
%%   failure_fact(Commit, Seq, SuiteTerm, Title)
%% Suite terms: suite_vitest | suite_e2e('e2e/<spec>.spec.ts') | ...
:- module(quasar_ui_run_ledger,
    [ load_run_ledger/1,       % +RunsDir
      reset_run_ledger/0,
      latest_result/3,         % +Commit, +SuiteTerm, -Result (pass|fail)
      commit_failures/2,       % +Commit, -Pairs (failure(Suite, Title))
      commit_health/2,         % +Commit, -Health (green|red)
      first_failing_commit/3,  % +SuiteTerm, +OrderedCommits, -Commit
      record_consultation/4,   % +RunsDir, +Hyp, +Outcome, +Note
      chain_record/3,          % +Hyp, -Confirmed, -Refuted
      suspect_chain/1,         % +Hyp (latest consultation is a refutation)
      chain_hypotheses/1       % -Hyps (distinct consulted hypotheses)
    ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- dynamic run_fact/7, failure_fact/4, consultation_fact/4.

reset_run_ledger :-
    retractall(run_fact(_, _, _, _, _, _, _)),
    retractall(failure_fact(_, _, _, _)),
    retractall(consultation_fact(_, _, _, _)).

%% load_run_ledger(+RunsDir): read every run-*.pl file in the directory
%% (sorted, read-only) and assert its facts.
load_run_ledger(Dir) :-
    reset_run_ledger,
    format(atom(Pattern), '~a/run-*.pl', [Dir]),
    expand_file_name(Pattern, Files),
    forall(member(File, Files), load_run_file(File)),
    format(atom(ConsultFile), '~a/expert-consultations.pl', [Dir]),
    ( exists_file(ConsultFile) -> load_run_file(ConsultFile) ; true ).

load_run_file(File) :-
    setup_call_cleanup(
        open(File, read, Stream),
        load_terms(Stream),
        close(Stream)).

load_terms(Stream) :-
    repeat,
    catch(read_term(Stream, Term, []), _, Term = end_of_file),
    ( Term == end_of_file -> !
    ; assert_term(Term), fail
    ).

assert_term(test_run(Commit, Seq, Suite, Result, Passed, Failed, When)) :- !,
    assertz(run_fact(Commit, Seq, Suite, Result, Passed, Failed, When)).
assert_term(test_failure(Commit, Seq, Suite, Title)) :- !,
    assertz(failure_fact(Commit, Seq, Suite, Title)).
%% Consultations: written by zara-expert confirm/refute. Latest (last
%% asserted) outcome per hypothesis is what suspect_chain/1 reads.
assert_term(consultation(Hyp, Outcome, Note, When)) :- !,
    assertz(consultation_fact(Hyp, Outcome, Note, When)).
assert_term(_).

%% latest_result(+Commit, +SuiteTerm, -Result): the highest-seq run of the
%% suite at the commit (reruns supersede earlier runs).
latest_result(Commit, Suite, Result) :-
    aggregate_all(max(Seq), run_fact(Commit, Seq, Suite, _, _, _, _), MaxSeq),
    run_fact(Commit, MaxSeq, Suite, Result, _, _, _).

is_latest_run(Commit, Seq, Suite) :-
    \+ ( run_fact(Commit, Seq2, Suite, _, _, _, _), Seq2 > Seq ).

%% commit_failures(+Commit, -Pairs): titles of failing suites' tests at
%% their latest run.
commit_failures(Commit, Pairs) :-
    findall(failure(Suite, Title),
            ( run_fact(Commit, Seq, Suite, fail, _, _, _),
              is_latest_run(Commit, Seq, Suite),
              failure_fact(Commit, Seq, Suite, Title) ),
            Pairs).

%% commit_health(+Commit, -Health): green when at least one run exists and
%% no suite's latest run failed; red when any suite's latest run failed.
commit_health(Commit, green) :-
    run_fact(Commit, _, _, _, _, _, _),
    \+ ( run_fact(Commit, Seq, Suite, fail, _, _, _),
         is_latest_run(Commit, Seq, Suite) ).
commit_health(Commit, red) :-
    run_fact(Commit, Seq, Suite, fail, _, _, _),
    is_latest_run(Commit, Seq, Suite).

%% first_failing_commit(+SuiteTerm, +OrderedCommits, -Commit): the earliest
%% commit in the caller-supplied git-log order whose latest run of the
%% suite failed - the regression entry point.
first_failing_commit(_, [], _) :- fail.
first_failing_commit(Suite, [Commit | _], Commit) :-
    latest_result(Commit, Suite, fail), !.
first_failing_commit(Suite, [_ | Rest], Commit) :-
    first_failing_commit(Suite, Rest, Commit).

%% ---------------------------------------------------------------
%% Expert-accuracy tracking: chains are claims, and claims get refuted.
%% record_consultation/4 appends to <RunsDir>/expert-consultations.pl
%% (agent accountability: confirm requires acting evidence, refute
%% requires a note).

record_consultation(RunsDir, Hyp, Outcome, Note) :-
    member(Outcome, [confirmed, refuted, partial]),
    get_time(Now),
    stamp_date_time(Now, Stamp, local),
    format(atom(When), '~w', [Stamp]),
    format(atom(File), '~a/expert-consultations.pl', [RunsDir]),
    setup_call_cleanup(
        ( open(File, append, Stream),
          current_output(Old), set_output(Stream) ),
        ( write_term(Stream, consultation(Hyp, Outcome, Note, When),
                     [quoted(true), spacing(next_argument)]),
          put_char(Stream, '.'), nl(Stream) ),
        ( set_output(Old), close(Stream) )),
    assertz(consultation_fact(Hyp, Outcome, Note, When)).

%% chain_record(+Hyp, -Confirmed, -Refuted)
chain_record(Hyp, Confirmed, Refuted) :-
    aggregate_all(count, consultation_fact(Hyp, confirmed, _, _), Confirmed),
    aggregate_all(count, consultation_fact(Hyp, refuted, _, _), Refuted).

latest_consultation_outcome(Hyp, Outcome) :-
    findall(Seen, consultation_fact(Hyp, Seen, _, _), Outcomes),
    Outcomes \= [],
    last(Outcomes, Outcome).

%% suspect_chain(+Hyp): the LATEST consultation for the chain is a
%% refutation. Diagnose warns at point of use; a later confirmation
%% clears the flag (history is kept, not deleted).
suspect_chain(Hyp) :-
    latest_consultation_outcome(Hyp, refuted).

chain_hypotheses(Hyps) :-
    setof(H, O^N^W^consultation_fact(H, O, N, W), Hyps).
