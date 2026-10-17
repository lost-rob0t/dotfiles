:- module(dotfiles_symbolic_chat_expert,
    [ answer/3, topic/3, provider_policy/1, max_model_calls/1, model_calls/1 ]).

:- use_module(topics, [topic/3, phrase_topic/2]).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).

answer(Question, Reply, evidence(topic(Topic), source(Source))) :-
    string(Question),
    string_length(Question, Length),
    Length > 0,
    Length =< 512,
    string_lower(Question, Lower),
    split_string(Lower, " \t\n?!.,:;()", " \t\n?!.,:;()", WordStrings),
    maplist(atom_string, Words, WordStrings),
    Words \== [],
    findall(Match, phrase_topic(Words, Match), Matches),
    sort(Matches, [Topic]),
    topic(Topic, Reply, Source),
    !.
