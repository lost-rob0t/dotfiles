:- module(design_conformance, [verify/3, lint/2]).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(crypto)).
:- use_module(library(readutil)).

% Specs are a finite, ground Prolog vocabulary. Never consult/call their terms.
read_spec(Path, Terms) :-
    setup_call_cleanup(open(Path, read, S, [encoding(utf8)]), read_terms(S, Terms), close(S)),
    maplist(valid_term, Terms),
    findall(Id, member(design(Id), Terms), [_]),
    findall(P-H, member(source_document(P,H), Terms), [_]),
    findall(Id, member(requirement(Id), Terms), Ids),
    Ids \== [], sort(Ids, Unique), same_length(Ids, Unique),
    forall(member(Id, Ids),
        (findall(O-L, member(source(Id,O,L),Terms), [_]), once(rule_for(Terms, Id)))),
    forall(member(source(Id,_,_),Terms), memberchk(requirement(Id),Terms)),
    forall((member(T, Terms), rule_id(T, Id)), memberchk(requirement(Id), Terms)).

read_terms(S, Terms) :-
    read_term(S, T, [syntax_errors(error)]),
    ( T == end_of_file -> Terms = []
    ; Terms = [T|Rest], read_terms(S, Rest) ).

valid_term(T) :-
    must_be(ground, T),
    ( allowed(T) -> true ; domain_error(design_spec_term, T) ).
allowed(design(Id)) :- atom(Id).
allowed(source_document(Path, Hash)) :- atom(Path), atom(Hash).
allowed(requirement(Id)) :- atom(Id).
allowed(source(Id, OrgId, Line)) :- atom(Id), atom(OrgId), integer(Line), Line > 0.
allowed(requires(Id, Fact)) :- atom(Id), valid_fact(Fact).
allowed(forbids(Id, Fact)) :- atom(Id), valid_fact(Fact).
allowed(implies(Id, A, B)) :- atom(Id), valid_fact(A), valid_fact(B).
allowed(single_value(Id, S, P)) :- maplist(atom, [Id,S,P]).
allowed(forbid_reachable(Id, P, A, B)) :- maplist(atom, [Id,P,A,B]).

valid_fact(fact(S,P,V)) :- maplist(atom, [S,P,V]).
rule_id(requires(Id,_), Id).
rule_id(forbids(Id,_), Id).
rule_id(implies(Id,_,_), Id).
rule_id(single_value(Id,_,_), Id).
rule_id(forbid_reachable(Id,_,_,_), Id).
rule_for(Terms, Id) :- member(T, Terms), rule_id(T, Id).

lint(Spec, Summary) :-
    read_spec(Spec, Terms),
    findall(Id, member(requirement(Id), Terms), Ids),
    length(Ids, N), memberchk(design(Id),Terms),
    memberchk(source_document(Path,Hash),Terms),
    findall(_{requirement:R,org_id:O,line:L}, member(source(R,O,L),Terms), Refs),
    Summary = lint(design(Id), source(Path,Hash), requirements(N), provenance(Refs)).

verify(Spec, Candidate, report(Status, Findings)) :-
    read_spec(Spec, Terms),
    valid_candidate(Candidate),
    memberchk(design(Id), Terms),
    crypto_file_hash(Spec, Hash, [algorithm(sha256)]),
    Candidate = candidate(CandidateId, CandidateHash, Facts, Scopes),
    ( Id == CandidateId, Hash == CandidateHash ->
      findall(Finding, finding(Terms, Facts, Scopes, Finding), Raw),
      sort(Raw, Findings), status(Findings, Status)
    ; Status = conflict, Findings = [conflict(design_binding, expected(Id,Hash))] ).

valid_candidate(candidate(Id,Hash,Facts,Scopes)) :-
    must_be(atom, Id), must_be(atom, Hash),
    must_be(list, Facts), must_be(list, Scopes),
    maplist(valid_fact, Facts),
    forall(member(scope(S,P), Scopes), maplist(atom,[S,P])),
    forall(member(T,Scopes), (T = scope(_,_))).

status(Findings, conflict) :- member(conflict(_,_), Findings), !.
status(Findings, unknown) :- member(unknown(_,_), Findings), !.
status(_, conformant).

finding(T, F, S, unknown(Id, missing(fact(A,P,V)))) :-
    member(requires(Id,fact(A,P,V)), T), \+ memberchk(fact(A,P,V), F),
    \+ memberchk(scope(A,P),S).
finding(T, F, S, conflict(Id, missing_required(fact(A,P,V)))) :-
    member(requires(Id,fact(A,P,V)), T), \+ memberchk(fact(A,P,V), F),
    memberchk(scope(A,P),S).
finding(T, F, _, conflict(Id, forbidden(Fact))) :-
    member(forbids(Id,Fact), T), memberchk(Fact,F).
finding(T, _, S, unknown(Id, incomplete_scope(A,P))) :-
    member(forbids(Id,fact(A,P,_)), T), \+ memberchk(scope(A,P),S).
finding(T, F, S, conflict(Id, implication(A,fact(B,P,V)))) :-
    member(implies(Id,A,fact(B,P,V)), T), memberchk(A,F),
    memberchk(scope(B,P),S), \+ memberchk(fact(B,P,V),F).
finding(T, F, S, unknown(Id, implication(A,fact(B,P,V)))) :-
    member(implies(Id,A,fact(B,P,V)), T), memberchk(A,F),
    \+ memberchk(scope(B,P),S), \+ memberchk(fact(B,P,V),F).
finding(T, _, S, unknown(Id, incomplete_scope(A,P))) :-
    member(implies(Id,fact(A,P,_),_), T), \+ memberchk(scope(A,P),S).
finding(T, F, _, conflict(Id, multiple_values(A,P,Values))) :-
    member(single_value(Id,A,P),T),
    findall(V,member(fact(A,P,V),F),Raw), sort(Raw,Values), length(Values,N), N > 1.
finding(T, _, S, unknown(Id, incomplete_scope(A,P))) :-
    member(single_value(Id,A,P),T), \+ memberchk(scope(A,P),S).
finding(T, F, _, conflict(Id, path(Path))) :-
    member(forbid_reachable(Id,P,A,B),T), path(F,P,A,B,[A],Path).
finding(T, _, S, unknown(Id, incomplete_relation(P))) :-
    member(forbid_reachable(Id,P,_,_),T), \+ memberchk(scope('*',P),S).

% Breadth-first graph search returns one shortest witness and visits nodes once.
path(F,P,A,B,_,Path) :-
    findall([Next,A],member(fact(A,P,Next),F),Queue),
    path_queue(F,P,Queue,[A],B,Reverse), reverse(Reverse,Path).

path_queue(_,_,[[B|Rest]|_],_,B,[B|Rest]) :- !.
path_queue(F,P,[[A|Rest]|Queue],Seen,B,Path) :-
    ( memberchk(A,Seen) -> path_queue(F,P,Queue,Seen,B,Path)
    ; findall([Next,A|Rest],member(fact(A,P,Next),F),Linked),
      append(Queue,Linked,More), path_queue(F,P,More,[A|Seen],B,Path) ).
