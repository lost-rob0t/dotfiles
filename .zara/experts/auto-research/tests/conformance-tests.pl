:- use_module(library(plunit)).
:- use_module('../kb/conformance').
:- use_module(library(crypto)).

spec(Path, Hash) :-
    tmp_file_stream(text,Path,S),
    format(S,"design(demo).~nsource_document('roam/design/demo.org',hash).~nrequirement(owner).~nsource(owner, 'org-id', 12).~nrequires(owner,fact(core,owner,server)).~nsingle_value(owner,core,owner).~nrequirement(no_cloud).~nsource(no_cloud,'org-id',20).~nforbid_reachable(no_cloud,calls,core,cloud).~n",[]),
    close(S), crypto_file_hash(Path,Hash,[algorithm(sha256)]).

:- begin_tests(design_conformance).
test(satisfying_complete_projection, [setup(spec(P,H)), cleanup(delete_file(P))]) :-
    verify(P,candidate(demo,H,[fact(core,owner,server)],[scope(core,owner),scope('*',calls)]),report(conformant,[])).
test(missing_requirement_is_unknown, [setup(spec(P,H)), cleanup(delete_file(P))]) :-
    verify(P,candidate(demo,H,[],[]),report(unknown,F)),
    assertion(member(unknown(owner,missing(fact(core,owner,server))),F)).
test(explicit_wrong_owner_conflicts, [setup(spec(P,H)), cleanup(delete_file(P))]) :-
    verify(P,candidate(demo,H,[fact(core,owner,client)],[scope(core,owner)]),report(conflict,F)),
    assertion(member(conflict(owner,missing_required(fact(core,owner,server))),F)).
test(contradictory_owners, [setup(spec(P,H)), cleanup(delete_file(P))]) :-
    verify(P,candidate(demo,H,[fact(core,owner,server),fact(core,owner,client)],[]),report(conflict,F)),
    assertion(member(conflict(owner,multiple_values(core,owner,[client,server])),F)).
test(transitive_violation_with_cycle_has_witness, [setup(spec(P,H)), cleanup(delete_file(P))]) :-
    verify(P,candidate(demo,H,[fact(core,calls,adapter),fact(adapter,calls,core),fact(adapter,calls,cloud)],[]),report(conflict,F)),
    assertion(member(conflict(no_cloud,path([core,adapter,cloud])),F)).
test(stale_or_other_design_rejected, [setup(spec(P,_)), cleanup(delete_file(P))]) :-
    verify(P,candidate(demo,stale,[],[]),report(conflict,_)).
test(forbidden_fact, [setup(spec(P,H)), cleanup(delete_file(P))]) :-
    setup_call_cleanup(open(P,append,S), format(S,'forbids(no_cloud,fact(core,provider,cloud)).~n',[]), close(S)),
    crypto_file_hash(P,H2,[algorithm(sha256)]), H \== H2,
    verify(P,candidate(demo,H2,[fact(core,provider,cloud)],[]),report(conflict,F)),
    assertion(member(conflict(no_cloud,forbidden(fact(core,provider,cloud))),F)).
test(implication_is_unknown_until_consequence_scope_complete, [setup(spec(P,_)), cleanup(delete_file(P))]) :-
    setup_call_cleanup(open(P,append,S), format(S,'implies(owner,fact(core,mode,write),fact(core,behavior,validates)).~n',[]), close(S)),
    crypto_file_hash(P,H,[algorithm(sha256)]),
    verify(P,candidate(demo,H,[fact(core,mode,write)],[]),report(unknown,_)),
    verify(P,candidate(demo,H,[fact(core,mode,write)],[scope(core,behavior)]),report(conflict,F)),
    assertion(member(conflict(owner,implication(_,_)),F)).
test(directives_never_execute, [setup(tmp_file_stream(text,P,S)), cleanup(delete_file(P)), throws(error(domain_error(design_spec_term,_),_))]) :-
    format(S,':- halt(0).~n',[]),close(S),lint(P,_).
test(nonground_spec_rejected, [setup(tmp_file_stream(text,P,S)), cleanup(delete_file(P)), throws(error(instantiation_error,_))]) :-
    format(S,'design(X).~n',[]),close(S),lint(P,_).
test(empty_spec_rejected, [setup(tmp_file_stream(text,P,S)), cleanup(delete_file(P)), fail]) :-
    close(S),lint(P,_).
:- end_tests(design_conformance).
:- initialization(main,main).
main :- (run_tests -> halt(0); halt(1)).
