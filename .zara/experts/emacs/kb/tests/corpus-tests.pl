:- use_module(library(plunit)).
:- use_module('../expert').
:- use_module('../corpus').

:- begin_tests(emacs_kb).
test(corpus_identity) :-
    emacs_corpus(Id, Version, Source, 'core-loaded'),
    atom_length(Id, 64), atom(Version), atom(Source).
test(command_documentation) :-
    describe('forward-char', function, Text, evidence(_, Digest, _)),
    atom(Text), atom_length(Digest, 64).
test(bounded_commands) :-
    commands(3, Rows), length(Rows, 3).
test(bounded_search) :-
    search('forward', 2, Rows), length(Rows, 2).
test(zero_limit, [throws(error(domain_error(result_limit, 0), _))]) :-
    commands(0, _).
test(unbounded_query, [throws(error(instantiation_error, _))]) :-
    search(_, 1, _).
test(too_large_limit, [throws(error(domain_error(result_limit, 101), _))]) :-
    search('forward', 101, _).
test(unknown_symbol, [fail]) :-
    describe('zara-no-such-symbol-fixture', function, _, _).
:- end_tests(emacs_kb).
