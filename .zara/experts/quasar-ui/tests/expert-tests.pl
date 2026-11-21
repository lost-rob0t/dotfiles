:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module('../kb/expert').
:- use_module('../corpus/build_corpus').
:- use_module('../kb/run_ledger').

%% Chain tests pin determinism as well as correctness: exactly one
%% hypothesis must fire per symptom set (no cross-chain false positives),
%% and the probe/verification pair must be solvable.
:- begin_tests(quasar_ui_chains, [cleanup(reset_facts)]).

%% Each chain test resets the fact store itself: block-level setup runs
%% only once, and hypothesis/1 enumerates every fact-backed rule, so
%% leftover facts from a previous test would cross-contaminate.
test(menu_vanish_chain, [setup(reset_facts)]) :-
    observe(surface, graph),
    observe(menu_opens_on_context_click, true),
    observe(item_press_unmounts_menu, true),
    findall(H, hypothesis(H), [pointerdown_close_guard_unexempted]),
    next_probe(pointerdown_close_guard_unexempted,
               grep_close_guards_for_menu_class_exemption),
    verification(pointerdown_close_guard_unexempted,
                 surface_e2e('graph-cascade-menu.spec.ts')), !.
test(menu_press_kept_no_hypothesis, [setup(reset_facts)]) :-
    observe(surface, graph),
    observe(menu_opens_on_context_click, true),
    observe(item_press_unmounts_menu, false),
    findall(H, hypothesis(H), []).

test(escape_backout_chain, [setup(reset_facts)]) :-
    observe(surface, graph),
    observe(submenu_backout_closes_whole_menu, true),
    findall(H, hypothesis(H), [duplicate_escape_owner]),
    next_probe(duplicate_escape_owner,
               grep_window_keydown_handlers_clearing_menu_state), !.

test(deleted_state_chain, [setup(reset_facts)]) :-
    observe(runtime_error, reference_error),
    observe(state_variable_removed, true),
    observe(typecheck_clean, true),
    findall(H, hypothesis(H), [stale_deleted_state_reference]),
    next_probe(stale_deleted_state_reference,
               grep_component_for_state_name_and_setter), !.

test(cross_scope_chain_requires_other_component_decl, [setup(reset_facts)]) :-
    observe(runtime_error, reference_error),
    findall(H, hypothesis(H), []).
test(cross_scope_chain, [setup(reset_facts)]) :-
    observe(runtime_error, reference_error),
    observe(symbol_declared_in_other_component, true),
    findall(H, hypothesis(H), Hs),
    memberchk(cross_scope_state_declaration, Hs),
    verification(cross_scope_state_declaration,
                 surface_e2e('settings-server-list.spec.ts')), !.

test(e2e_dark_chain_binds_known_surface, [setup(reset_facts)]) :-
    observe(forgejo_gate, green),
    observe(live_stack_error, present),
    observe(changed_surface, graph),
    findall(H, hypothesis(H), [e2e_dark_in_gate]),
    next_probe(e2e_dark_in_gate,
               run_surface_e2e_against_live_stack(graph)), !.
test(e2e_dark_chain_unknown_surface, [setup(reset_facts)]) :-
    observe(forgejo_gate, green),
    observe(live_stack_error, present),
    observe(changed_surface, no_such_surface),
    findall(H, hypothesis(H), []).

test(stale_class_chain, [setup(reset_facts)]) :-
    observe(component_class_renamed, true),
    observe(interaction_broken_after_rename, true),
    findall(H, hypothesis(H), [stale_class_selectors]),
    next_probe(stale_class_selectors, grep_old_and_new_class_across_src).

test(view_index_chain_requires_zero_rows, [setup(reset_facts)]) :-
    observe(surface, documents),
    observe(server_deployed_recently, true),
    observe(server_rows_rendered, 25),
    findall(H, hypothesis(H), []).
test(view_index_chain, [setup(reset_facts)]) :-
    observe(surface, documents),
    observe(server_deployed_recently, true),
    observe(server_rows_rendered, 0),
    findall(H, hypothesis(H), [view_index_rebuilding]),
    next_probe(view_index_rebuilding, probe_documents_recent_total_rows).

test(spawned_stack_collision_chain, [setup(reset_facts)]) :-
    observe(stack, spawned_webserver),
    observe(spawn_error, eaddrinuse),
    findall(H, hypothesis(H), [spawned_stack_port_collision]),
    next_probe(spawned_stack_port_collision,
               diff_live_stack_ports_and_spawn_with_quasar_mcp_0).
test(spawn_collision_needs_eaddrinuse, [setup(reset_facts)]) :-
    observe(stack, spawned_webserver),
    observe(spawn_error, connection_refused),
    findall(H, hypothesis(H), []).

test(fresh_stack_red_chain, [setup(reset_facts)]) :-
    observe(stack, spawned_webserver),
    observe(journeys_green_live_stack, true),
    observe(journeys_red_spawned_stack, true),
    findall(H, hypothesis(H), [fresh_stack_journeys_red]),
    next_probe(fresh_stack_journeys_red,
               read_recorded_journey_failure_messages).

:- end_tests(quasar_ui_chains).

:- begin_tests(quasar_ui_surface_registry, [setup(reset_facts), cleanup(reset_facts)]).

test(graph_menu_family_registered) :-
    journeys_for_surface(graph, Specs),
    length(Specs, 7),
    forall(member(Spec, ['graph-cascade-menu.spec.ts',
                         'graph-object-type-picker.spec.ts',
                         'editor-focus.spec.ts']),
           member(Spec, Specs)).
test(documents_surface_registered) :-
    journeys_for_surface(documents, Specs),
    length(Specs, 2),
    memberchk('documents-recents.spec.ts', Specs).
test(registry_covers_all_26_tracked_specs) :-
    % Snapshot contract: the full inventory is 26 tracked spec files. When
    % a journey spec is added, register it AND bump this pin; the corpus
    % builder's unmapped_specs/3 audit reports any drift against a real
    % checkout.
    findall(Spec, surface_spec(_, Spec), Specs0),
    sort(Specs0, Specs),
    length(Specs, 26).
test(declared_gaps_closed_and_surface_remapped) :-
    surface_spec(documents_editor, 'canonical-documents.spec.ts'),
    surface_spec(agents, 'agents-hub.spec.ts'),
    \+ surface_uncovered(_, _).
test(unknown_surface_empty) :-
    journeys_for_surface(unknown, []).

:- end_tests(quasar_ui_surface_registry).

:- begin_tests(quasar_ui_corpus_parser).

fixture_lines([
    "// Operator journey (forgejo #142): empty query shows server recents",
    "// under the recent-ingest divider, paged by a key cursor.",
    "import { expect, test } from \"./fixtures\";",
    "",
    "test(\"empty query lists server recents\", async ({ page }) => {",
    "  await expect(divider).toContainText(\"recent ingest\");",
    "});",
    "",
    "test(\"pages by cursor\", async ({ page }) => {",
    "  await loadMore.click();",
    "});"
]).
header_only_lines([
    "// Just a description, no tests yet.",
    "export const unused = 1;"
]).

test(parses_header_and_titles_in_order) :-
    fixture_lines(Lines),
    parse_spec_source(Lines, journey(Header, Titles)),
    atom_string(Header, 'Operator journey (forgejo #142): empty query shows server recents under the recent-ingest divider, paged by a key cursor.'),
    Titles == ["empty query lists server recents", "pages by cursor"].
test(no_titles_yields_empty) :-
    header_only_lines(Lines),
    parse_spec_source(Lines, journey(_, [])).

test(build_corpus_reads_a_real_file) :-
    setup_tmp_spec(Dir, SpecFile),
    build_corpus(Dir, [journey('tmp-fixture', Header, Titles)]),
    atom_string(Header, 'Fixture journey.'),
    Titles == ["fixture test one"],
    teardown_tmp_spec(Dir, SpecFile).

:- end_tests(quasar_ui_corpus_parser).

%% build_corpus scans <Root>/frontend/e2e/, so the fixture must live at
%% that depth inside the temp tree.
setup_tmp_spec(Dir, SpecFile) :-
    tmp_file(quasar_ui_corpus, Dir),
    format(atom(Frontend), '~a/frontend', [Dir]),
    format(atom(E2e), '~a/e2e', [Frontend]),
    make_directory(Dir),
    make_directory(Frontend),
    make_directory(E2e),
    format(atom(SpecFile), '~a/tmp-fixture.spec.ts', [E2e]),
    atomics_to_string([
        "// Fixture journey.",
        "import { test } from \"./fixtures\";",
        "test(\"fixture test one\", async () => {});"
    ], "\n", Source),
    open(SpecFile, write, Stream),
    write(Stream, Source),
    close(Stream).

teardown_tmp_spec(Dir, SpecFile) :-
    catch(delete_file(SpecFile), _, true),
    format(atom(Frontend), '~a/frontend', [Dir]),
    format(atom(E2e), '~a/e2e', [Frontend]),
    catch(delete_directory(E2e), _, true),
    catch(delete_directory(Frontend), _, true),
    catch(delete_directory(Dir), _, true).

%% Run-ledger reader: facts written by quasar scripts/record-run.mjs into
%% .prolog/runs/run-<HEAD>.pl. Fixtures mirror that format exactly.
:- begin_tests(quasar_ui_run_ledger, [setup(reset_run_ledger), cleanup(reset_run_ledger)]).

setup_ledger(Dir, Files) :-
    tmp_file(quasar_ui_ledger, Dir),
    make_directory(Dir),
    write_run_file(Dir, 'run-c1.pl', [
        "test_run('c1', 1, suite_vitest, pass, 505, 0, '2026-11-21T02:00:00Z')."
    ]),
    write_run_file(Dir, 'run-c2.pl', [
        "test_run('c2', 1, suite_vitest, pass, 505, 0, '2026-11-21T02:30:00Z').",
        "test_run('c2', 1, suite_e2e('e2e/graph-cascade-menu.spec.ts'), pass, 3, 0, '2026-11-21T02:31:00Z')."
    ]),
    write_run_file(Dir, 'run-c3.pl', [
        "test_run('c3', 1, suite_vitest, pass, 505, 0, '2026-11-21T03:00:00Z').",
        "test_run('c3', 1, suite_e2e('e2e/graph-cascade-menu.spec.ts'), fail, 2, 1, '2026-11-21T03:01:00Z').",
        "test_failure('c3', 1, suite_e2e('e2e/graph-cascade-menu.spec.ts'), 'cascade closes on a press outside the menu').",
        "test_run('c3', 2, suite_e2e('e2e/graph-cascade-menu.spec.ts'), pass, 3, 0, '2026-11-21T03:10:00Z')."
    ]),
    Files = ['run-c1.pl', 'run-c2.pl', 'run-c3.pl'].

write_run_file(Dir, Name, Lines) :-
    format(atom(File), '~a/~a', [Dir, Name]),
    atomics_to_string(Lines, "\n", Body),
    open(File, write, Stream),
    write(Stream, Body), nl(Stream),
    close(Stream).

teardown_ledger(Dir, Files) :-
    forall(member(F, Files),
           ( format(atom(Path), '~a/~a', [Dir, F]),
             catch(delete_file(Path), _, true) )),
    catch(delete_directory(Dir), _, true).

test(ledger_loads_and_reports_commit_health) :-
    setup_ledger(Dir, Files),
    load_run_ledger(Dir),
    commit_health(c1, green),
    commit_health(c2, green),
    commit_health(c3, green),  % rerun (seq 2) superseded the failure
    !,
    teardown_ledger(Dir, Files).

test(latest_result_superseded_by_rerun) :-
    setup_ledger(Dir, Files),
    load_run_ledger(Dir),
    \+ latest_result(c3, suite_e2e('e2e/graph-cascade-menu.spec.ts'), fail),
    latest_result(c3, suite_e2e('e2e/graph-cascade-menu.spec.ts'), pass),
    teardown_ledger(Dir, Files).

test(red_when_latest_run_failed) :-
    setup_ledger(Dir, Files),
    load_run_ledger(Dir),
    % Append a later failing run of vitest at c3: latest seq wins -> red.
    format(atom(File), '~a/run-c3.pl', [Dir]),
    open(File, append, Stream),
    write(Stream, "test_run('c3', 2, suite_vitest, fail, 490, 15, '2026-11-21T03:20:00Z')."), nl(Stream),
    write(Stream, "test_failure('c3', 2, suite_vitest, 'auth regression breaks CP login')."), nl(Stream),
    close(Stream),
    reset_run_ledger,
    load_run_ledger(Dir),
    commit_health(c3, red),
    latest_result(c3, suite_vitest, fail),
    commit_failures(c3, Pairs),
    memberchk(failure(suite_vitest, 'auth regression breaks CP login'), Pairs),
    commit_failures(c3, Pairs2),
    forall(member(failure(S, _), Pairs2), S == suite_vitest),
    teardown_ledger(Dir, Files).

test(first_failing_commit_over_ordered_history) :-
    setup_ledger(Dir, Files),
    load_run_ledger(Dir),
    % vitest passes c1..c3; regression enters at c4 (fixture appended).
    format(atom(File), '~a/run-c4.pl', [Dir]),
    open(File, write, Stream),
    write(Stream, "test_run('c4', 1, suite_vitest, fail, 400, 105, '2026-11-21T04:00:00Z')."), nl(Stream),
    close(Stream),
    reset_run_ledger,
    load_run_ledger(Dir),
    first_failing_commit(suite_vitest, [c1, c2, c3, c4], c4),
    \+ first_failing_commit(suite_vitest, [c1, c2, c3], _),
    teardown_ledger(Dir, Files).

:- end_tests(quasar_ui_run_ledger).

%% Expert-accuracy: consultations (confirm/refute) recorded through the
%% same ledger; a latest-refutation flags the chain SUSPECT for diagnose.
:- begin_tests(quasar_ui_consultations, [setup(reset_run_ledger), cleanup(reset_run_ledger)]).

setup_consultations(Dir, Files) :-
    tmp_file(quasar_ui_ledger, Dir),
    make_directory(Dir),
    format(atom(File), '~a/run-c1.pl', [Dir]),
    open(File, write, Stream),
    write(Stream, "test_run('c1', 1, suite_vitest, pass, 505, 0, '2026-11-21T02:00:00Z')."),
    nl(Stream),
    close(Stream),
    Files = [File].

test(record_refute_reconfirm_cycle, [setup(reset_run_ledger)]) :-
    setup_consultations(Dir, Files),
    writeln("DBG t1 setup done"),
    record_consultation(Dir, 'test_chain', confirmed, 'evidence A'),
    writeln("DBG t1 rec1 done"),
    record_consultation(Dir, 'test_chain', refuted, 'wrong on nested menus'),
    reset_run_ledger,
    load_run_ledger(Dir),
    chain_record('test_chain', 1, 1),
    suspect_chain('test_chain'),
    chain_hypotheses(['test_chain']),
    record_consultation(Dir, 'test_chain', confirmed, 're-confirmed after revision'),
    reset_run_ledger,
    load_run_ledger(Dir),
    chain_record('test_chain', 2, 1),
    \+ suspect_chain('test_chain'),
    !,
    teardown_consultations(Dir, Files).

test(partial_consultation_is_valid, [setup(reset_run_ledger)]) :-
    setup_consultations(Dir, Files),
    record_consultation(Dir, 'test_chain', partial, 'fixed the guard, missed the escape handler'),
    reset_run_ledger,
    load_run_ledger(Dir),
    chain_record('test_chain', 0, 0),
    \+ suspect_chain('test_chain'),
    format(atom(ConsultFile), '~a/expert-consultations.pl', [Dir]),
    read_file_to_string(ConsultFile, Stored, []),
    sub_atom(Stored, _, _, _, "partial"),
    sub_atom(Stored, _, _, _, "missed the escape handler"),
    !,
    teardown_consultations(Dir, Files).

teardown_consultations(Dir, Files) :-
    forall(member(F, Files),
           ( format(atom(Path), '~a/~a', [Dir, F]),
             catch(delete_file(Path), _, true) )),
    catch(delete_directory(Dir), _, true).

:- end_tests(quasar_ui_consultations).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
