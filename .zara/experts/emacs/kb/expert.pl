:- module(zara_emacs_expert, [describe/4, commands/2, search/3]).
:- use_module(corpus).
:- use_module(library(error)).
:- use_module(library(solution_sequences)).

% These read-only helpers do not activate experts or authorize editor effects.
describe(Name, Kind, Text, evidence(Corpus, Digest, Library)) :-
    must_be(atom, Name),
    must_be(atom, Kind),
    emacs_symbol(Corpus, Name, Kind, _, Library, documented),
    emacs_documentation(Corpus, Name, Kind, Text, Digest).

commands(Limit, Matches) :-
    result_limit(Limit),
    findnsols(Limit, command(Name, Corpus, Library),
              emacs_symbol(Corpus, Name, function, true, Library, _), Matches), !.

search(Query, Limit, Matches) :-
    must_be(atom, Query),
    atom_length(Query, Length),
    (between(1, 256, Length) -> true ; domain_error(search_length, Length)),
    result_limit(Limit),
    downcase_atom(Query, Needle),
    findnsols(Limit, match(Name, Kind, Corpus, Digest),
              matching_document(Needle, Name, Kind, Corpus, Digest), Matches), !.

matching_document(Needle, Name, Kind, Corpus, Digest) :-
    emacs_documentation(Corpus, Name, Kind, Text, Digest),
    downcase_atom(Name, LowerName),
    downcase_atom(Text, LowerText),
    once((sub_atom(LowerName, _, _, _, Needle) ; sub_atom(LowerText, _, _, _, Needle))).

result_limit(Limit) :-
    must_be(integer, Limit),
    (between(1, 100, Limit) -> true ; domain_error(result_limit, Limit)).
