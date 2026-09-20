:- module(crew_protocol_v1,
          [ crew_protocol/1, crew_phase/2, crew_transition/4,
            crew_spawn_allowed/3, crew_invariant/2 ]).
:- use_module(library(lists)).

% Append-only, Git-owned knowledge. Future incompatible rules use a new module.
% Provenance: dotfiles#278/#281 and operator corrections in this conversation.
crew_protocol('ZARA-CREW/1').
crew_phase(0, research).
crew_phase(1, adversarial_review).
crew_phase(2, analysis).
crew_phase(3, design).
crew_phase(4, design_review).
crew_phase(5, promotion).
crew_phase(6, develop).
crew_phase(7, verify).
crew_phase(8, completed).

crew_invariant(history, preserve_existing_prolog_bytes).
crew_invariant(storage, git_tracked_append_only).
crew_invariant(knowledge, pinned_shared_source_not_prompt_authority).
crew_invariant(spawn, child_authority_subset_of_parent).
crew_invariant(budget, debit_parent_before_child_admission).
crew_invariant(cancellation, descendant_cascade_and_generation_fence).
crew_invariant(identity, independent_of_role_and_project).
crew_invariant(evidence, exact_artifact_revision).

% Review and approval evidence is supplied only by the trusted coordinator.
% Model-written booleans and peer messages are not accepted as gate receipts.
crew_transition(From, To, Receipt, true) :-
    crew_phase(N, From), M is N+1, crew_phase(M, To),
    Receipt.verified == true,
    string(Receipt.artifact), string_length(Receipt.artifact, Length), Length > 0,
    transition_receipt(From, Receipt), !.
crew_transition(_, _, _, false).

transition_receipt(adversarial_review, R) :- !, R.independent == true.
transition_receipt(design_review, R) :- !, R.independent == true.
transition_receipt(promotion, R) :- !, R.operator_approved == true.
transition_receipt(_, _).

crew_spawn_allowed(Parent, Child, Limits) :-
    memberchk(Child.role, Parent.delegates),
    is_list(Parent.capabilities), is_list(Child.capabilities),
    forall(member(Capability, Child.capabilities),
           memberchk(Capability, Parent.capabilities)),
    integer(Child.turns), Child.turns > 0,
    integer(Parent.remaining), Child.turns =< Parent.remaining,
    integer(Parent.depth), Parent.depth < Limits.max_depth,
    integer(Parent.children), Parent.children < Limits.max_children,
    integer(Limits.members), Limits.members < Limits.max_agents.
