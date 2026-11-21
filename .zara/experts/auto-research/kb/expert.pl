:- module(auto_research_expert,
    [ relevant/4, evidence/4, plan/4, constraint/4, provider_policy/1, max_model_calls/1 ]).
:- use_module(library(lists)).
:- use_module(library(error)).

provider_policy(disabled).
max_model_calls(0).

% KB is an explicitly loaded, trusted data module. No source text is called.
relevant(KB, Project, Tags, Sources) :-
    must_be(atom, KB), must_be(atom, Project), must_be(list(atom), Tags),
    findall(source(Path, Hash),
        ( KB:document(Path, _, Project, _, _, Hash),
          ( Tags == [] -> true ; member(Tag, Tags), KB:tag(Path, Tag) ) ),
        Raw),
    sort(Raw, Sources).

evidence(KB, Path, Needle, evidence(Path, Hash, Line, Text)) :-
    must_be(atom, KB), must_be(atom, Needle),
    KB:document(Path, _, _, _, _, Hash),
    KB:passage(Path, Line, Text),
    downcase_atom(Text, Lower), downcase_atom(Needle, Query),
    sub_atom(Lower, _, _, _, Query).

phase(design, [resolve_current_schema_lock, trace_requirements_to_sources,
               inspect_current_source_and_tests, identify_ownership_boundaries,
               define_inputs_outputs_and_failure_states,
               define_acceptance_checks, obtain_design_review]).
phase(implement, [resolve_current_schema_lock, inspect_current_source_and_tests,
                  confirm_design_approval_for_current_content,
                  add_regression_fixture, implement_smallest_canonical_path,
                  observe_real_checks, verify_persisted_outputs_when_applicable,
                  update_project_kb_and_documentation]).

% This is an advisory plan, never permission to implement or deploy.
plan(KB, Path, Phase, plan(Actions, Blockers, Sources)) :-
    must_be(atom, KB), must_be(atom, Path), must_be(atom, Phase),
    phase(Phase, BaseActions),
    KB:document(Path, _, _, _, _, _),
    closure(KB, [Path], [], Paths, LimitBlockers),
    findall(source(P, H),
            (member(P, Paths), KB:document(P, _, _, _, _, H)), RawSources),
    sort(RawSources, Sources),
    findall(inspect_constraint(Kind, source(P, Hash, Line)),
        (P = Path, constraint(KB, P, Kind, evidence(P, Hash, Line, _))), RawConstraints),
    sort(RawConstraints, Constraints),
    append(BaseActions, Constraints, Actions),
    findall(B, (member(P, Paths), blocker(KB, P, B)), RawBlockers),
    append([approval_evidence_required|RawBlockers], LimitBlockers, AllBlockers),
    sort(AllBlockers, Blockers).

% Bound default agent context; callers can query linked sections separately.
closure(_, [], Seen, Seen, []).
closure(KB, [Path|Rest], Seen, Result, Limits) :-
    ( memberchk(Path, Seen) -> closure(KB, Rest, Seen, Result, Limits)
    ; length(Seen, 16) -> Result = Seen, Limits = [source_limit_reached(16)]
    ; findall(Next, linked(KB, Path, Next), Raw), sort(Raw, Linked),
      append(Rest, Linked, Queue),
      closure(KB, Queue, [Path|Seen], Result, Limits) ).

linked(KB, Path, Next) :-
    KB:reference(Path, _, Target), atom_concat('id:', Id, Target),
    KB:org_id(Next, _, Id).

linked(KB, Path, Next) :-
    KB:file_link(Path, _, Next), KB:document(Next, _, _, _, _, _).

blocker(KB, Path, review_claim(State)) :-
    KB:metadata(Path, approval_state, State), State \== 'APPROVED'.
blocker(KB, Path, lifecycle_requires_review(Path, State)) :-
    KB:document(Path, _, _, _, State, _),
    memberchk(State, ['REJECTED', 'SUPERSEDED', 'BLOCKED', 'MISSING']).
blocker(KB, Path, unresolved_id(Path, Line, Id)) :-
    KB:reference(Path, Line, Target), atom_concat('id:', Id, Target),
    \+ KB:org_id(_, _, Id).
blocker(KB, Path, ambiguous_id(Id)) :-
    KB:org_id(Path, _, Id),
    findall(P-L, KB:org_id(P, L, Id), Matches),
    length(Matches, Count), Count > 1.

blocker(KB, Path, unresolved_file(Path, Line, Next)) :-
    KB:file_link(Path, Line, Next), \+ KB:document(Next, _, _, _, _, _).

constraint(KB, Path, Kind, evidence(Path, Hash, Line, Text)) :-
    KB:document(Path, _, _, _, _, Hash),
    KB:heading(Path, Line, _, Title),
    downcase_atom(Title, Lower),
    constraint_kind(Kind, Cue), sub_atom(Lower, _, _, _, Cue),
    KB:passage(Path, Line, Text).

constraint_kind(requirement, requirement).
constraint_kind(invariant, invariant).
constraint_kind(ownership, ownership).
constraint_kind(boundary, boundary).
constraint_kind(acceptance, acceptance).
constraint_kind(failure, failure).
