:- use_module('kb/conformance').
:- use_module(library(http/json)).
:- use_module(library(readutil)).

main([lint, Spec]) :-
    lint(Spec, lint(design(Id),source(Path,Hash),requirements(N),provenance(Refs))),
    json_write_dict(current_output, _{design_id:Id,source_path:Path,source_sha256:Hash,requirements:N,sources:Refs}), nl, !.
main([verify, Spec, Proposal]) :-
    setup_call_cleanup(open(Proposal,read,S,[encoding(utf8)]), json_read_dict(S,D), close(S)),
    atom_string(Id,D.design_id), atom_string(Hash,D.spec_sha256),
    maplist(triple,D.assertions,Facts), maplist(pair,D.complete_scopes,Scopes),
    verify(Spec,candidate(Id,Hash,Facts,Scopes),Report),
    portray_clause(Report),
    ( Report = report(conformant,_) -> true ; halt(1) ), !.
main(_) :- halt(2).
triple([S,P,V],fact(A,B,C)) :- maplist(atom_string,[A,B,C],[S,P,V]).
pair([S,P],scope(A,B)) :- maplist(atom_string,[A,B],[S,P]).
:- initialization(main,main).
