%% Quasar UI bug-hunting expert: symptom -> hypothesis -> probe ->
%% verification chains over the desktop Quasar UI surface.
%%
%% Authored expert rules (dotfiles ownership). Incident knowledge is
%% distilled from the quasar repository's durable KB
%% (.prolog/kb/ui_e2e_harness.pl, .prolog/kb/testing-pitfalls.pl) and from
%% live operator incidents (forgejo quasar PRs #147/#148/#149, 2026-11-20).
%% The corpus builder reads the quasar checkout live through an explicit
%% path argument; no project facts are copied into this pack.
%%
%% Clauses are grouped per predicate (hypothesis/1, next_probe/2,
%% verification/2), one chain per clause, in matching order. The chain
%% comment names each group.
:- module(dotfiles_quasar_ui_expert,
    [ fact/2, observe/2, reset_facts/0,
      hypothesis/1, next_probe/2, verification/2,
      surface_spec/2, surface_route/2, surface_uncovered/2,
      journeys_for_surface/2 ]).

:- dynamic fact/2.

observe(Key, Value) :-
    retractall(fact(Key, _)),
    assertz(fact(Key, Value)).

reset_facts :- retractall(fact(_, _)).

%% surface_spec(Surface, SpecFile) - the persisted Playwright journey specs
%% (quasar frontend/e2e) that exercise each UI surface. The gate for every
%% verification below. Full inventory 2026-11-20: every tracked spec file
%% is mapped, and routes without dedicated journeys are declared
%% surface_uncovered rather than silently missing.
surface_spec(graph, 'graph-cascade-menu.spec.ts').
surface_spec(graph, 'graph-object-type-picker.spec.ts').
surface_spec(graph, 'graph-controls.spec.ts').
surface_spec(graph, 'graph-viewport.spec.ts').
surface_spec(graph, 'graph-fullscreen.spec.ts').
surface_spec(graph, 'graph-destructive-ops.spec.ts').
surface_spec(graph, 'editor-focus.spec.ts').
surface_spec(documents, 'canonical-documents.spec.ts').
surface_spec(documents, 'documents-recents.spec.ts').
surface_spec(documents_editor, 'canonical-documents.spec.ts').
surface_spec(agents, 'agents-hub.spec.ts').
surface_spec(datasets, 'dataset-picker.spec.ts').
surface_spec(datasets, 'datasets-pwa.spec.ts').
surface_spec(import, 'large-import.spec.ts').
surface_spec(import, 'wireless-imports.spec.ts').
surface_spec(actors, 'actor-settings.spec.ts').
surface_spec(actors, 'live-actor-discovery.spec.ts').
surface_spec(actors, 'target-actors.spec.ts').
surface_spec(code, 'code-studio.spec.ts').
surface_spec(settings, 'settings-server-list.spec.ts').
surface_spec(stats_home, 'view-installation.spec.ts').
surface_spec(application_shell, 'application.spec.ts').
surface_spec(application_shell, 'route-density.spec.ts').
surface_spec(application_shell, 'ui-consistency.spec.ts').
surface_spec(mobile, 'mobile-navigation.spec.ts').
surface_spec(mobile, 'mobile-fit-agent-disclosure.spec.ts').
surface_spec(mobile, 'mobile-pan-after-actor.spec.ts').

%% surface_route(Surface, Route): the app routes (frontend/src/App.jsx)
%% behind each surface.
surface_route(stats_home, '/').
surface_route(graph, '/graph').
surface_route(datasets, '/datasets').
surface_route(documents, '/documents').
surface_route(import, '/import').
surface_route(agents, '/agents').
surface_route(actors, '/actors').
surface_route(code, '/code').
surface_route(settings, '/settings').

%% surface_uncovered(Surface, Route): defined features with NO journey
%% spec yet. These are honest gaps, not coverage. Declared dynamic: the
%% empty state (all surfaces covered, 2026-11-21) is meaningful; new
%% gaps get authored facts HERE rather than omitted surfaces.
:- dynamic surface_uncovered/2.


journeys_for_surface(Surface, Specs) :-
    findall(Spec, surface_spec(Surface, Spec), Specs).

%% ---------------------------------------------------------------
%% Chain 1 (2026-11-20, quasar PR #147/#149): the cascade opens on
%% right-click but pressing any item unmounts it before the click can
%% act. Root cause each time so far: a stage-level pointerdown
%% close-guard whose closest() selector exempts only a stale menu
%% class (pointerdown precedes click).
hypothesis(pointerdown_close_guard_unexempted) :-
    fact(surface, graph),
    fact(menu_opens_on_context_click, true),
    fact(item_press_unmounts_menu, true).

%% Chain 2: Escape backs out of a submenu level only when CascadingMenu
%% is the sole Escape owner; blanket window keydown handlers that clear
%% menu state defeat back-out and close the whole cascade.
hypothesis(duplicate_escape_owner) :-
    fact(surface, graph),
    fact(submenu_backout_closes_whole_menu, true).

%% Chain 3 (quasar KB testing-pitfalls: jsx_checkjs_blind_spot): tsc
%% --noEmit does not flag undefined identifiers in .jsx (checkJs off for
%% JavaScript sources): deleting component state while missing a setter
%% reference passes typecheck and lint, then dies at runtime.
hypothesis(stale_deleted_state_reference) :-
    fact(runtime_error, reference_error),
    fact(state_variable_removed, true),
    fact(typecheck_clean, true).

%% Chain 4 (quasar PR #148): ReferenceError on a symbol whose
%% declaration lives in a sibling component (state declared in
%% ImportPage, JSX read it in SettingsPage) - the JSX compiles in
%% isolation but the scope never binds at runtime.
hypothesis(cross_scope_state_declaration) :-
    fact(runtime_error, reference_error),
    fact(symbol_declared_in_other_component, true).

%% Chain 5 (quasar KB ui_e2e_harness: forgejo_e2e_dark): the Forgejo
%% gates never run the Playwright chain (gated to GitHub), so green CI
%% covers nothing about UI interactions; menu/dialog changes need the
%% affected e2e specs run against the live stack before merging.
hypothesis(e2e_dark_in_gate) :-
    fact(forgejo_gate, green),
    fact(live_stack_error, present),
    fact(changed_surface, Surface),
    once(surface_spec(Surface, _)).

%% Chain 6 (quasar KB ui_e2e_harness: class_rename_guard_drift):
%% renaming a component/container class without updating every guard,
%% bridge, or enhancer selector that matches it - the old class
%% survives in close-guards and mutation observers that then no-op.
hypothesis(stale_class_selectors) :-
    fact(component_class_renamed, true),
    fact(interaction_broken_after_rename, true).

%% Chain 7 (quasar PR #445/#446 deploy, 2026-11-20): the server section
%% of the Documents view is empty right after a star-server deploy -
%% boot re-pushes design docs and resets every CouchDB view index;
%% totals read 0 or flicker mid-rebuild. Not a UI defect.
hypothesis(view_index_rebuilding) :-
    fact(surface, documents),
    fact(server_deployed_recently, true),
    fact(server_rows_rendered, 0).

%% ---------------------------------------------------------------
%% Probes, in the same order as the hypotheses above.
next_probe(pointerdown_close_guard_unexempted,
           grep_close_guards_for_menu_class_exemption).
next_probe(duplicate_escape_owner,
           grep_window_keydown_handlers_clearing_menu_state).
next_probe(stale_deleted_state_reference,
           grep_component_for_state_name_and_setter).
next_probe(cross_scope_state_declaration,
           locate_declaring_component_of_symbol).
next_probe(e2e_dark_in_gate,
           run_surface_e2e_against_live_stack(Surface)) :-
    fact(changed_surface, Surface).
next_probe(stale_class_selectors,
           grep_old_and_new_class_across_src).
next_probe(view_index_rebuilding,
           probe_documents_recent_total_rows).

%% ---------------------------------------------------------------
%% Verifications, in the same order as the hypotheses above.
verification(pointerdown_close_guard_unexempted,
             surface_e2e('graph-cascade-menu.spec.ts')).
verification(duplicate_escape_owner,
             surface_e2e('graph-cascade-menu.spec.ts')).
verification(stale_deleted_state_reference,
             vitest_plus_live_reload).
verification(cross_scope_state_declaration,
             surface_e2e('settings-server-list.spec.ts')).
verification(e2e_dark_in_gate,
             surface_e2e_family_green_live).
verification(stale_class_selectors,
             surface_e2e('graph-cascade-menu.spec.ts')).
verification(view_index_rebuilding,
             skip_gated_recents_spec_after_index_ready).
