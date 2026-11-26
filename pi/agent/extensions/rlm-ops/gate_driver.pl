% Fail-closed verification gate for the pi rlm-ops /symbolic mode.
%
% Usage (arguments come from the GATE_ARGS environment variable as a JSON
% array [ObservationsFile, VerifyFile]; positional .pl arguments would be
% consulted by swipl, which would execute directives from the files):
%
%   GATE_ARGS='["obs.pl","verify.pl"]' swipl -q gate_driver.pl
%
% observations.pl is machine-written by the extension: obs/5 facts
%   obs(N, tool(Tool), summary(Atom), ok(Bool), stamp(Number))
% verify.pl is agent-written via the verify_require/verify_rules tools:
%   requirement(Name, Goal)  — Goal must succeed for the gate to pass
%   plus any derivation clauses the agent declared.
%
% Everything is loaded term-by-term (directives are rejected and fail the
% gate) and each requirement is evaluated independently; a goal that throws
% or fails is reported, never silently accepted. Exit 0 only when every
% requirement is proven. No call_with_time_limit: it breaks findall
% backtracking on the arm64-android build; the caller enforces timeouts.

:- module(gate_driver, [main/0]).

:- use_module(library(http/json)).
:- use_module(library(lists)).

:- initialization(main, main).

:- dynamic obs/5.
:- dynamic requirement/2.

main :-
    (   catch(run, E, (fail_closed(E), fail))
    ->  halt(0)
    ;   halt(1)
    ).

fail_closed(E) :-
    message_to_string(E, Msg),
    respond(_{ok:false, kind:"gate_error", error:Msg}).

run :-
    getenv("GATE_ARGS", ArgsJson),
    setup_call_cleanup(
        open_string(ArgsJson, S),
        json_read_dict(S, [ObsFile, VerifyFile]),
        close(S)),
    load_observations(ObsFile),
    load_verify(VerifyFile),
    evaluate(Report),
    respond(Report),
    Report.ok == true.

% ------------------------------------------------------------- observations

load_observations(File) :-
    (   exists_file(File)
    ->  load_facts(File, obs_clause)
    ;   true
    ).

obs_clause(T) :-
    T = obs(_, _, _, _, _),
    !,
    assertz(T).
obs_clause(T) :-
    throw(error(domain_error(observation_fact_expected, T), _)).

% ------------------------------------------------------------------- verify

load_verify(File) :-
    (   exists_file(File)
    ->  load_facts(File, verify_clause)
    ;   true
    ).

verify_clause(T) :-
    functor(T, ':-', 1),
    !,
    throw(error(domain_error(verify_directive_not_allowed, T), _)).
verify_clause(T) :-
    functor(T, ':', 2),
    !,
    throw(error(domain_error(verify_module_term_not_allowed, T), _)).
verify_clause(T) :-
    functor(T, '-->', 2),
    !,
    throw(error(domain_error(verify_dcg_not_allowed, T), _)).
verify_clause(T) :-
    assertz(T).

% Load terms one by one so agent rules become dynamic clauses without ever
% executing directives.
load_facts(File, Accept) :-
    setup_call_cleanup(
        open(File, read, S),
        load_stream(S, Accept),
        close(S)).

load_stream(S, Accept) :-
    read_term(S, T, []),
    T \== end_of_file,
    !,
    call(Accept, T),
    load_stream(S, Accept).
load_stream(_, _).

% --------------------------------------------------------------- evaluation

evaluate(Report) :-
    set_prolog_flag(unknown, error),
    findall(requirement(Name, Goal), requirement(Name, Goal), Reqs),
    count_obs(Obs),
    maplist(eval_req, Reqs, Results),
    partition_results(Results, Passed, Failed),
    length(Reqs, Total),
    length(Passed, PassCount),
    (   Failed == []
    ->  Ok = true
    ;   Ok = false
    ),
    Report = _{ok:Ok,
               kind:"gate",
               requirements:Total,
               passed:PassCount,
               observations:Obs,
               failed:Failed}.

count_obs(N) :-
    findall(o, clause(obs(_, _, _, _, _), true), Os),
    length(Os, N).

% One evaluation per requirement: the inner catch converts a thrown goal into
% failed(Reason); success and failure both surface as a single thrown marker
% captured by the outer catch, so the reason is kept without re-running the goal.
eval_req(requirement(Name, Goal), Result) :-
    term_string(Goal, GoalText, [quoted(true), numbervars(true)]),
    catch(
        (   catch(Goal, GE,
                  (   message_to_string(GE, GM),
                      throw(failed(GM))))
        ->  throw(passed)
        ;   throw(failed("goal was not proven from recorded observations"))
        ),
        Outcome, true),
    outcome(Outcome, Name, GoalText, Result).

outcome(passed, Name, GoalText, _{name:Name, goal:GoalText, outcome:"pass"}).
outcome(failed(Reason), Name, GoalText,
        _{name:Name, goal:GoalText, outcome:"fail", reason:Reason}).

partition_results(Results, Passed, Failed) :-
    include_pass(Results, Passed),
    exclude_pass(Results, Failed).

include_pass([], []).
include_pass([R|T0], [R|T]) :- R.outcome == "pass", !, include_pass(T0, T).
include_pass([_|T0], T) :- include_pass(T0, T).

exclude_pass([], []).
exclude_pass([R|T0], [R|T]) :- R.outcome \== "pass", !, exclude_pass(T0, T).
exclude_pass([_|T0], T) :- exclude_pass(T0, T).

respond(Dict) :-
    json_write_dict(current_output, Dict, [width(0)]),
    nl,
    flush_output(current_output).
