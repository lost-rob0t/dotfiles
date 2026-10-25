:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(symbolic_chat_expert).

test(offline_mode_answer_has_provenance_and_zero_model_budget) :-
    answer("Does local AI work offline?", Reply, Evidence),
    sub_string(Reply, _, _, _, "on this device"),
    Evidence = evidence(topic(local_ai), source('wiki/android-local-ai.org')),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(editable_studio_answer) :-
    answer("How do I edit Prolog rules?", Reply, evidence(topic(prolog_studio), _)),
    sub_string(Reply, _, _, _, "Logic"),
    sub_string(Reply, _, _, _, "Save & reload").

test(remote_mode_answer) :-
    answer("How do I use a remote API?", Reply, evidence(topic(remote_ai), _)),
    sub_string(Reply, _, _, _, "Remote").

test(symbolic_mode_does_not_claim_effect_execution) :-
    answer("What can symbolic mode do?", Reply, evidence(topic(symbolic_mode), _)),
    sub_string(Reply, _, _, _, "approval").

test(covered_topics_are_ground_and_sourced) :-
    findall(Topic, topic(Topic, _, _), Topics),
    length(Topics, Count),
    Count >= 12,
    forall(topic(_, Reply, Source),
           ( string(Reply),
             string_length(Reply, Length),
             Length > 20,
             atom(Source),
             file_name_extension(_, Extension, Source),
             member(Extension, [pl, org])
           )).

test(ambiguous_question_fails_closed, [fail]) :-
    answer("How do I edit Prolog rules and use a remote API?", _, _).

test(import_answer_does_not_claim_model_is_installed) :-
    answer("How do I import a model?", Reply, evidence(topic(model_import), _)),
    sub_string(Reply, _, _, _, "import a compatible").

test(unknown_symbolic_question_has_no_model_fallback) :-
    answer("What happens for an unsupported symbolic question?", Reply,
           evidence(topic(symbolic_unknown), _)),
    sub_string(Reply, _, _, _, "does not silently ask a model").

test(unknown_question_fails_closed, [fail]) :-
    answer("Who won the lunar chess championship?", _, _).

test(empty_question_fails_closed, [fail]) :-
    answer(" ", _, _).

test(overlong_question_fails_closed, [fail]) :-
    length(Codes, 513),
    maplist(=(0'a), Codes),
    string_codes(Question, Codes),
    answer(Question, _, _).

:- end_tests(symbolic_chat_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
