% KB linter used by scripts/check-kb-syntax.
% Flags unquoted atoms containing dots: foo(a.b) consults cleanly in SWI but
% the term is really compound(.)/2 (dict-access syntax) and every query over
% it raises "Type error: `dict' expected". This bit the osint_ops.pl lesson
% (curt.ns.cloudflare.com) on 2026-11-26; this linter makes it a gate.

kb_lint_file(Path) :-
    setup_call_cleanup(open(Path, read, S),
                       kb_lint_stream(S, Path),
                       close(S)).

kb_lint_stream(S, Path) :-
    catch(read_term(S, T, [syntax_errors(fail)]),
          E, (format(atom(Msg), '~w: syntax error: ~w', [Path, E]),
              throw(error(Msg)))),
    (   T == end_of_file
    ->  true
    ;   kb_lint_term(T, Path),
        kb_lint_stream(S, Path)
    ).

% Walk the term tree; flag compound(.)/2 anywhere (dict-access atoms).
kb_lint_term(T, _) :- var(T), !.
kb_lint_term(T, _) :- atom(T), !.
kb_lint_term(T, _) :- number(T), !.
kb_lint_term(T, _) :- string(T), !.
kb_lint_term(T, _) :- is_list(T), !.
kb_lint_term(T, Path) :-
    compound_name_arguments(T, (.), Args), !,
    format('ERROR ~w: unquoted atom with dots parsed as dict syntax: ~w~n', [Path, T]),
    maplist(kb_lint_term_(Path), Args).
kb_lint_term(T, Path) :-
    compound_name_arguments(T, _, Args),
    maplist(kb_lint_term_(Path), Args).

kb_lint_term_(Path, T) :- kb_lint_term(T, Path).
