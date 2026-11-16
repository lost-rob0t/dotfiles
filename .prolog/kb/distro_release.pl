% distro_release.pl -- the dotfiles meta-distro release-engineering system.
% Built and verified 2026-11-16 (run-a8d450e): sandbox suite 25/25,
% check-literate-sync green, engine proven over the prolog MCP.

% Canonical parts (details: docs/wiki/distro-release-engineering.org).
distro_part('issues.org', 'human intake (Inbox one-liners) + tracked requirements').
distro_part('scripts/distro', 'harness-agnostic CLI; literate source scripts/distro.org').
distro_part('distro/config.pl', 'human-owned policy: council_quorum/1, attack_step/1; agents never weaken it').
distro_part('distro/engine', 'SWI-Prolog engine: engine.pl loader, model/gates/version/report').
distro_part('distro/state', 'generated facts + append-only journal; untracked; never hand-edited').
distro_part('distro/archive', 'released milestones, changelog, durable releases.pl').

% Rules agents must follow.
distro_rule(intake_is_one_liners_in_issues_org_inbox_or_distro_feat_bug_never_tasks_org).
distro_rule(only_distro_triage_mints_issue_ids).
distro_rule(claim_before_working_with_a_stable_agent_id).
distro_rule(evidence_is_recorded_through_distro_verify_not_asserted).
distro_rule(done_requires_green_evidence_before_a_release_gate_passes).
distro_rule(dependencies_are_declared_facts_via_distro_dep).
distro_rule(votes_and_attacks_are_epoch_stamped_and_void_when_issues_org_changes).
distro_rule(release_requires_engine_release_ready_not_council_opinion).
distro_rule(released_requirements_are_archived_and_stop_being_tracked_never_hand_deleted).
distro_rule(council_reviews_and_attacks_code_before_prs).

% Engine contract (verified over prolog MCP session 7445a2985587).
engine_recipe(load, '[distro/engine/engine] derives the root from its own location or DISTRO_ROOT; reload_distro_state after any distro command').
engine_recipe(query, 'release_blocker(M, B) yields one answer per blocking reason; release_ready/1 fails closed; next_issue/1 orders by severity rank').
engine_gotcha(tangle_needs_relative_path, 'org-babel-tangle-file silently writes NOTHING when given an absolute path from this setup; run from the repo root with a relative path').
engine_gotcha(sort_arity, 'SWI has msort/2 and sort/2, no msort/1 or sort/1; nondeterministic milestone_work/2 inside gate clauses needs once/1 to avoid duplicate blockers').
engine_gotcha(property_header_tangle, 'a stray language keyword inside a header-args property value (e.g. ":bash :tangle ...") breaks org tangle silently; use per-block :tangle like dotfiles-sync.org').

% Test/attack suite.
distro_attack_target('tests/distro-test.sh', 'end-to-end sandbox suite (DISTRO_ROOT); 25 assertions over the full lifecycle').
