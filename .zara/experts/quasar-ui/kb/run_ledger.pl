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
      first_failing_commit/3   % +SuiteTerm, +OrderedCommits, -Commit
    ]).

:- use_module(library(apply)).
:- dynamic run_fact/7, failure_fact/4.

reset_run_ledger :-
    retractall(run_fact(_, _, _, _, _, _, _)),
    retractall(failure_fact(_, _, _, _)).

%% load_run_ledger(+RunsDir): read every run-*.pl file in the directory
%% (sorted, read-only) and assert its facts.
load_run_ledger(Dir) :-
    reset_run_ledger,
    format(atom(Pattern), '~a/run-*.pl', [Dir]),
    expand_file_name(Pattern, Files),
    forall(member(File, Files), load_run_file(File)).

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
