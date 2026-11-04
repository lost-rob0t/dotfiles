:- use_module(library(plunit)).
:- use_module('../kb/expert').
:- dynamic document/6, metadata/3, tag/2, org_id/3, reference/3, passage/3, file_link/3, heading/4.

document(research, research, project, 'Research', 'VERIFIED', hash_r).
document(design, design, project, 'Design', 'APPROVED', hash_d).
document(other, research, unrelated, 'Other', 'DONE', hash_o).
metadata(research, approval_state, 'PENDING').
metadata(design, status, 'APPROVED').
tag(research, prolog).
tag(design, prolog).
org_id(design, 3, 'design-id').
reference(research, 9, 'id:design-id').
reference(design, 10, 'id:missing-id').
reference(design, 11, 'id:design-id').
heading(research, 8, 1, 'Replay requirements').
passage(research, 8, 'Use deterministic replay and bounded retries.').

:- begin_tests(auto_research_expert).
test(relevance_keeps_project_and_provenance) :-
    relevant(user, project, [prolog], Results),
    assertion(Results == [source(design,hash_d),source(research,hash_r)]).
test(lifecycle_never_becomes_approval) :-
    plan(user, research, implement, plan(_, Blockers, Sources)),
    assertion(member(review_claim('PENDING'), Blockers)),
    assertion(member(source(research, hash_r), Sources)).
test(approved_lifecycle_still_needs_evidence) :-
    plan(user, design, implement, plan(_, Blockers, _)),
    assertion(member(approval_evidence_required, Blockers)).
test(design_has_concrete_actions) :-
    plan(user, design, design, plan(Actions, _, _)),
    assertion(member(resolve_current_schema_lock, Actions)),
    assertion(member(trace_requirements_to_sources, Actions)).
test(implementation_has_concrete_actions) :-
    plan(user, design, implement, plan(Actions, _, _)),
    assertion(member(add_regression_fixture, Actions)),
    assertion(member(observe_real_checks, Actions)).
test(cycle_terminates_and_dangling_link_blocks) :-
    plan(user, research, design, plan(_, B, S)),
    assertion(member(unresolved_id(design,10,'missing-id'), B)),
    assertion(S == [source(design,hash_d),source(research,hash_r)]).
test(explanation_has_exact_passage_line_hash, [nondet]) :-
    evidence(user, research, replay, evidence(research,hash_r,8,Text)),
    assertion(Text == 'Use deterministic replay and bounded retries.').
test(plan_contains_literal_source_constraints) :-
    plan(user, research, design, plan(Actions, _, _)),
    assertion(member(inspect_constraint(requirement,source(research,hash_r,8)),Actions)).
test(unknown_document_fails, [fail]) :- plan(user, missing, design, _).
test(unknown_phase_fails, [fail]) :- plan(user, design, deploy, _).
test(bounded_plan_exposes_truncation,
     [setup(forall(between(1,20,N),
        (atom_concat(node,N,P),assertz(user:document(P,research,p,P,'REVIEW',hash)),
         assertz(user:file_link(research,N,P))))),
      cleanup((retractall(user:document(_,research,p,_,_,_)),retractall(user:file_link(research,_,_))))]) :-
    plan(user,research,design,plan(_,B,S)),length(S,16),
    assertion(member(source_limit_reached(16),B)).
test(no_models) :- provider_policy(disabled), max_model_calls(0).
:- end_tests(auto_research_expert).
:- initialization(main, main).
main :- (run_tests -> halt(0); halt(1)).
