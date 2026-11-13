:- module(devlog_kb, [main/0]).

:- use_module(library(http/json)).
:- use_module(library(lists)).
:- dynamic devlog_event/11.

load_facts(File) :-
    retractall(devlog_event(_,_,_,_,_,_,_,_,_,_,_)),
    consult(File).

event_dict(Id, Day, Provider, Repo, Kind, Number, Title, State, Url, UpdatedAt, RunId,
           _{id:Id, day:Day, provider:Provider, repo:Repo, kind:Kind, number:Number,
             title:Title, state:State, url:Url, updated_at:UpdatedAt, run_id:RunId}).

event(Dict) :-
    devlog_event(Id, Day, Provider, Repo, Kind, Number, Title, State, Url, UpdatedAt, RunId),
    event_dict(Id, Day, Provider, Repo, Kind, Number, Title, State, Url, UpdatedAt, RunId, Dict).

contains_ci(Haystack, Needle) :-
    downcase_atom(Haystack, H),
    downcase_atom(Needle, N),
    sub_atom(H, _, _, _, N).

matches(recent, _, _).
matches(day, Value, Dict) :- get_dict(day, Dict, X), contains_ci(X, Value).
matches(run, Value, Dict) :- get_dict(run_id, Dict, X), contains_ci(X, Value).
matches(open, _, Dict) :- get_dict(state, Dict, open).
matches(completed, _, Dict) :-
    get_dict(state, Dict, State),
    memberchk(State, [merged, closed, committed]).
matches(repo, Value, Dict) :- get_dict(repo, Dict, X), contains_ci(X, Value).
matches(provider, Value, Dict) :- get_dict(provider, Dict, X), contains_ci(X, Value).
matches(kind, Value, Dict) :- get_dict(kind, Dict, X), contains_ci(X, Value).
matches(state, Value, Dict) :- get_dict(state, Dict, X), contains_ci(X, Value).
matches(search, Value, Dict) :-
    ( get_dict(title, Dict, X), contains_ci(X, Value)
    ; get_dict(repo, Dict, X), contains_ci(X, Value)
    ; get_dict(provider, Dict, X), contains_ci(X, Value)
    ; get_dict(kind, Dict, X), contains_ci(X, Value)
    ; get_dict(state, Dict, X), contains_ci(X, Value)
    ; get_dict(number, Dict, X), contains_ci(X, Value)
    ).

limit_list(Limit, In, Out) :-
    length(Prefix, Limit),
    append(Prefix, _, In), !,
    Out = Prefix.
limit_list(_, In, In).

query_events(Mode, Value, Limit, Rows) :-
    findall(Dict, (event(Dict), matches(Mode, Value, Dict)), All),
    reverse(All, NewestFirst),
    limit_list(Limit, NewestFirst, Rows).

summary_dict(Dict) :-
    findall(P, devlog_event(_,_,P,_,_,_,_,_,_,_,_), Providers0),
    sort(Providers0, Providers),
    findall(R, devlog_event(_,_,_,R,_,_,_,_,_,_,_), Repos0),
    sort(Repos0, Repos),
    findall(K, devlog_event(_,_,_,_,K,_,_,_,_,_,_), Kinds0),
    sort(Kinds0, Kinds),
    findall(Id, devlog_event(Id,_,_,_,_,_,_,_,_,_,_), Ids),
    length(Ids, Count),
    Dict = _{schema:'org-prolog-devlog-kb/v1', events:Count,
             providers:Providers, repos:Repos, kinds:Kinds}.

run_query(summary, _, _, Result) :- !,
    summary_dict(Result).
run_query(Mode, Value, Limit, Result) :-
    query_events(Mode, Value, Limit, Rows),
    Result = _{schema:'org-prolog-devlog-kb/v1', mode:Mode, value:Value, events:Rows}.

main :-
    current_prolog_flag(argv, Args),
    ( Args = [Facts, Mode, Value, Limit0|_]
    -> atom_number(Limit0, Limit),
       load_facts(Facts),
       run_query(Mode, Value, Limit, Result),
       json_write_dict(current_output, Result, [width(0)]), nl
    ;  json_write_dict(current_output,
                       _{schema:'org-prolog-devlog-kb/v1', error:'expected facts mode value limit'},
                       [width(0)]), nl,
       halt(2)
    ).
