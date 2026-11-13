% Generic shared-blackboard driver for the pi rlm-ops extension.
%
% Every operation is a one-shot swipl invocation guarded by flock(1) by the
% caller, so this driver only needs single-process file semantics. The
% blackboard file holds plain Prolog facts; msg/6 and bb_seq/1 are the
% built-in mailbox vocabulary and any other asserted fact is shared state.
%
% Usage (arguments come from the KB_ARGS environment variable as a JSON
% array of strings, never from positional argv: swipl consults trailing .pl
% arguments, which would execute directives from untrusted board files):
%
%   KB_ARGS='["post","/path/board.pl","main","a1","task","done(3)"]' \
%     swipl -q kb_driver.pl
%
% Operations:
%   ping    -
%   post    <file> <from> <to> <kind> <payload>   append msg/6, returns seq
%   take    <file> <who|all>                      atomically drain mailbox
%   assert  <file> <term>                         append one shared fact
%   retract <file> <term>                         remove all matching facts
%   query   <file> <goal> <maxSolutions>          run a goal over the board
%   dump    <file>                                return the raw board text
%   parse   <term>                                validate one term/clause
%
% Output: one JSON object on stdout. Failures exit 1 (fail closed).
%
% Termux note: predicate_property/2 is unreliable on the arm64-android
% build (Name/Arity indicators resolve to unrelated yall imports), so the
% save path tracks shared predicates in board_pred/2 instead, and queries
% are unguarded by call_with_time_limit because that breaks findall
% backtracking on this build; the caller enforces timeouts by process kill.

:- module(kb_driver, [main/0]).

:- use_module(library(http/json)).
:- use_module(library(lists)).

:- initialization(main, main).

:- dynamic bb_seq/1.
:- dynamic msg/6.
:- dynamic board_pred/2.

main :-
    (   catch(run, E, (fail_closed(E), fail))
    ->  halt(0)
    ;   halt(1)
    ).

fail_closed(E) :-
    message_to_string(E, Msg),
    respond(_{ok:false, kind:"driver_error", error:Msg}).

run :-
    getenv("KB_ARGS", ArgsJson),
    setup_call_cleanup(
        open_string(ArgsJson, S),
        json_read_dict(S, Args0),
        close(S)),
    maplist(as_atom, Args0, Argv),
    Argv = [Op|Rest],
    dispatch(Op, Rest, R),
    respond(R).

as_atom(Value, Atom) :- atom_string(Atom, Value).

dispatch(ping, _, _{ok:true, kind:"ping", version:1}) :- !.
dispatch(post, [File, From, To, Kind, PayloadIn], R) :- !,
    board_file(File),
    load_board(File),
    payload_term(PayloadIn, Payload),
    next_seq(N),
    get_time(T),
    Stamp is round(T * 1000),
    assertz(msg(From, To, Kind, Payload, N, Stamp)),
    save_board(File),
    R = _{ok:true, kind:"post", seq:N}.
dispatch(take, [File, WhoIn], R) :- !,
    board_file(File),
    load_board(File),
    take_all(WhoIn, Taken),
    (   Taken == []
    ->  true
    ;   save_board(File)
    ),
    maplist(term_to_text, Taken, Texts),
    length(Taken, Count),
    R = _{ok:true, kind:"take", count:Count, messages:Texts}.
dispatch(assert, [File, TermIn], R) :- !,
    board_file(File),
    load_board(File),
    read_term_from_atom(TermIn, Term, []),
    (   board_fact(Term)
    ->  true
    ;   throw(error(domain_error(kb_fact_expected, Term), _))
    ),
    functor(Term, F, A),
    record_pred(F, A),
    assertz(Term),
    save_board(File),
    R = _{ok:true, kind:"assert", asserted:TermIn}.
dispatch(retract, [File, TermIn], R) :- !,
    board_file(File),
    load_board(File),
    read_term_from_atom(TermIn, Term, []),
    findall(Term, call(Term), Matches),
    length(Matches, Count),
    retractall(Term),
    (   Count > 0
    ->  save_board(File)
    ;   true
    ),
    R = _{ok:true, kind:"retract", retracted:Count}.
dispatch(query, [File, GoalIn, MaxIn], R) :- !,
    board_file(File),
    load_board(File),
    atom_number(MaxIn, Max),
    atom_to_term(GoalIn, Goal, _),
    (   catch((findall(Goal, Goal, All0),
               cap_solutions(All0, Max, All)), E,
              (   message_to_string(E, Msg),
                  All = error(Msg)))
    ->  true
    ;   All = []
    ),
    query_result(All, R).
dispatch(dump, [File], R) :- !,
    board_file(File),
    setup_call_cleanup(
        open(File, read, S),
        read_string(S, _, Text),
        close(S)),
    R = _{ok:true, kind:"dump", text:Text}.
dispatch(parse, [TermIn], R) :- !,
    catch(read_term_from_atom(TermIn, _Term, []), _, fail),
    R = _{ok:true, kind:"parse"}.
dispatch(Op, _, _) :-
    throw(error(domain_error(kb_driver_operation, Op), _)).

% ---------------------------------------------------------------- board I/O

board_file(File) :-
    (   exists_file(File)
    ->  true
    ;   file_directory_name(File, Dir),
        catch(make_directory(Dir), _, true),
        open(File, write, S),
        close(S)
    ).

% Load every fact of the board into this process. Terms are asserted one by
% one so shared agent facts become dynamic predicates; their functors are
% recorded in board_pred/2 for the save path. Directives and rules are
% rejected: the board is facts only.
load_board(File) :-
    board_file(File),
    setup_call_cleanup(
        open(File, read, S),
        load_terms(S),
        close(S)).

load_terms(S) :-
    read_term(S, T, []),
    T \== end_of_file,
    !,
    (   board_fact(T)
    ->  functor(T, F, A),
        record_pred(F, A),
        assertz(T)
    ;   throw(error(domain_error(kb_fact_expected, T), _))
    ),
    load_terms(S).
load_terms(_).

% The board is plain facts: directives (:-/1), rules (:-/2), module-qualified
% terms (:/2) and DCG rules (-->/2) are all rejected.
board_fact(T) :- functor(T, ':-', _), !, fail.
board_fact(T) :- functor(T, ':', 2), !, fail.
board_fact(T) :- functor(T, '-->', 2), !, fail.
board_fact(_).

record_pred(bb_seq, 1) :- !.
record_pred(msg, 6) :- !.
record_pred(board_pred, 2) :- !.
record_pred(F, A) :-
    clause(board_pred(F, A), true),
    !.
record_pred(F, A) :-
    assertz(board_pred(F, A)).

next_seq(N) :-
    (   bb_seq(N0)
    ->  true
    ;   N0 = 0
    ),
    N is N0 + 1,
    retractall(bb_seq(_)),
    assertz(bb_seq(N)).

% Rewrite the board from current dynamic state: mailbox vocabulary first,
% then every registered shared predicate.
save_board(File) :-
    atomic_list_concat([File, '.tmp'], Tmp),
    setup_call_cleanup(
        open(Tmp, write, S),
        (   forall(clause(bb_seq(N), true), format(S, "~q.~n", [bb_seq(N)])),
            forall(clause(msg(From, To, Kind, Payload, Seq, Stamp), true),
                   format(S, "~q.~n", [msg(From, To, Kind, Payload, Seq, Stamp)])),
            forall(clause(board_pred(F, A), true),
                   write_pred_clauses(S, F, A))
        ),
        close(S)),
    rename_file(Tmp, File).

write_pred_clauses(S, F, A) :-
    functor(Head, F, A),
    forall(clause(Head, Body),
           (   Body == true
           ->  format(S, "~q.~n", [Head])
           ;   format(S, "~q.~n", [(Head :- Body)])
           )).

% ---------------------------------------------------------------------- take

take_all(WhoIn, Taken) :-
    text(WhoIn, Who),
    (   Who == all
    ->  findall(M, nth_message(M), Rev)
    ;   findall(M, (nth_message(M), M = msg(_, To, _, _, _, _), text(To, Who)),
                Rev)
    ),
    reverse(Rev, Taken),
    maplist(retract_message, Taken).

nth_message(msg(From, To, Kind, Payload, Seq, Stamp)) :-
    clause(msg(From, To, Kind, Payload, Seq, Stamp), true).

retract_message(M) :-
    retract(M).

% -------------------------------------------------------------------- query

cap_solutions(All0, Max, All) :-
    length(All0, Total),
    (   Total =< Max
    ->  All = All0
    ;   list_prefix(All0, Max, All)
    ).

list_prefix(_, 0, []) :- !.
list_prefix([H|T], N, [H|P]) :-
    N > 0,
    N1 is N - 1,
    list_prefix(T, N1, P).

query_result(error(Msg), _{ok:false, kind:"query", error:Msg}) :- !.
query_result(Solutions, R) :- !,
    maplist(term_to_text, Solutions, Texts),
    length(Texts, Count),
    R = _{ok:true, kind:"query", count:Count, solutions:Texts}.

% ---------------------------------------------------------------- utilities

payload_term(PayloadIn, Payload) :-
    text(PayloadIn, Text),
    (   catch(read_term_from_atom(PayloadIn, Term, []), _, fail)
    ->  Payload = Term
    ;   Payload = Text
    ).

text(Value, Text) :-
    (   atom(Value) -> atom_string(Value, Text)
    ;   string(Value) -> Text = Value
    ;   term_string(Value, Text)
    ).

term_to_text(Term, Text) :-
    term_string(Term, Text, [quoted(true), numbervars(true)]).

respond(Dict) :-
    json_write_dict(current_output, Dict, [width(0)]),
    nl,
    flush_output(current_output).
