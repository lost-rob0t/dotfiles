:- begin_tests(mara_todo_expert).

:- use_module('../kb/expert.pl').

fixture(Path) :-
    prolog_load_context(directory, Directory),
    directory_file_path(Directory, 'fixtures/todo-kb.pl', Path).

test(read_generated_projection) :-
    fixture(Path),
    todo_rows_from_file(Path, Rows),
    length(Rows, 2).

test(active_filters_closed_state) :-
    fixture(Path),
    todo_rows_from_file(Path, Rows),
    todo_active_rows(Rows, Active),
    Active = [todo('task-next', 'NEXT', _, _, _, _, _, _)].

test(next_prefers_next_state) :-
    fixture(Path),
    todo_rows_from_file(Path, Rows),
    todo_next_row(Rows, todo('task-next', 'NEXT', _, _, _, _, _, _)).

test(rejects_executable_or_unknown_terms,
     [throws(error(domain_error(mara_todo_snapshot_term, _), _))]) :-
    tmp_file_stream(text, Path, Stream),
    format(Stream, 'shell_command(\'rm -rf /\').~n', []),
    close(Stream),
    call_cleanup(todo_rows_from_file(Path, _), delete_file(Path)).

:- end_tests(mara_todo_expert).

:- run_tests,
   halt.
