:- use_module('kb/expert').
:- use_module(library(process)).
:- use_module(library(filesex)).

% The command checks freshness before loading or reasoning over the mirror.
main([Root0, Phase, Document]) :-
    absolute_file_name(Root0, Root, [file_type(directory), access(read)]),
    directory_file_path(Root, 'scripts/check-prolog-mirror.py', Checker),
    process_create(path(python3), [Checker], [process(Pid)]),
    process_wait(Pid, exit(0)),
    directory_file_path(Root, '.prolog/kb/mirror/index.pl', Index),
    use_module(Index, []),
    plan(auto_research_kb, Document, Phase, Advice),
    portray_clause(Advice), !.
main(_) :-
    format(user_error, 'Usage: swipl -q -f none -s run.pl -- REPO design|implement roam/path.org~n', []),
    halt(1).
:- initialization(main, main).
