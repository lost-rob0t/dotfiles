:- module(mara_todo_expert, [
    todo_snapshot_path/1,
    todo_rows/1,
    todo_rows_from_file/2,
    todo_active/1,
    todo_active_rows/2,
    todo_next/1,
    todo_next_row/2,
    todo_scheduled_on/2
]).

todo_snapshot_path(Path) :-
    getenv('MARA_TODO_PROLOG_KB', Path),
    Path \== '',
    !.
todo_snapshot_path(Path) :-
    getenv('HOME', Home),
    atom_concat(Home, '/.cache/zara/mara/todo-kb.pl', Path).

todo_rows(Rows) :-
    todo_snapshot_path(Path),
    ( exists_file(Path) ->
        todo_rows_from_file(Path, Rows)
    ; Rows = []
    ).

todo_rows_from_file(Path, Rows) :-
    setup_call_cleanup(
        open(Path, read, Stream, [encoding(utf8)]),
        read_snapshot(Stream, Rows),
        close(Stream)
    ).

read_snapshot(Stream, Rows) :-
    read_term(Stream, Term, [syntax_errors(error)]),
    read_snapshot_term(Term, Stream, Rows).

read_snapshot_term(end_of_file, _, []) :-
    !.
read_snapshot_term(todo_generation(Generation), Stream, Rows) :-
    !,
    atom(Generation),
    read_snapshot(Stream, Rows).
read_snapshot_term(
    todo(Id, State, Title, File, Scheduled, Deadline, Priority, Tags),
    Stream,
    [todo(Id, State, Title, File, Scheduled, Deadline, Priority, Tags)|Rows]
) :-
    !,
    maplist(atom, [Id, State, Title, File, Scheduled, Deadline, Priority, Tags]),
    read_snapshot(Stream, Rows).
read_snapshot_term(Term, _, _) :-
    throw(error(domain_error(mara_todo_snapshot_term, Term), _)).

closed_state('DONE').
closed_state('CANCELLED').
closed_state('NO').

todo_active_rows(Rows, Active) :-
    include(active_row, Rows, Active).

active_row(todo(_, State, _, _, _, _, _, _)) :-
    \+ closed_state(State).

todo_active(Active) :-
    todo_rows(Rows),
    todo_active_rows(Rows, Active).

state_rank('NEXT', 0).
state_rank('STRT', 1).
state_rank('LOOP', 2).
state_rank('PROJ', 3).
state_rank('TODO', 4).
state_rank('WAIT', 5).
state_rank('IDEA', 6).
state_rank(_, 100).

todo_next_row(Rows, Row) :-
    todo_active_rows(Rows, Active),
    map_list_to_pairs(row_rank, Active, Pairs),
    keysort(Pairs, [_-Row|_]).

row_rank(todo(_, State, _, _, _, _, _, _), Rank) :-
    state_rank(State, Rank).

todo_next(Row) :-
    todo_rows(Rows),
    todo_next_row(Rows, Row).

todo_scheduled_on(Date, ScheduledRows) :-
    atom(Date),
    atom_length(Date, 10),
    todo_rows(Rows),
    include(scheduled_on(Date), Rows, ScheduledRows).

scheduled_on(Date, todo(_, _, _, _, Scheduled, Deadline, _, _)) :-
    ( sub_atom(Scheduled, _, 10, _, Date)
    ; sub_atom(Deadline, _, 10, _, Date)
    ).
