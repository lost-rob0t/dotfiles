:- use_module(library(plunit)).
:- use_module('../kb/expert').
:- use_module('../corpus/build_corpus').

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
    observe(changed_surface, graph_menus),
    findall(H, hypothesis(H), [e2e_dark_in_gate]),
    next_probe(e2e_dark_in_gate,
               run_surface_e2e_against_live_stack(graph_menus)), !.
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
    observe(surface, documents_recents),
    observe(server_deployed_recently, true),
    observe(server_rows_rendered, 25),
    findall(H, hypothesis(H), []).
test(view_index_chain, [setup(reset_facts)]) :-
    observe(surface, documents_recents),
    observe(server_deployed_recently, true),
    observe(server_rows_rendered, 0),
    findall(H, hypothesis(H), [view_index_rebuilding]),
    next_probe(view_index_rebuilding, probe_documents_recent_total_rows).

:- end_tests(quasar_ui_chains).

:- begin_tests(quasar_ui_surface_registry, [setup(reset_facts), cleanup(reset_facts)]).

test(graph_menu_family_registered) :-
    journeys_for_surface(graph_menus, Specs),
    length(Specs, 6),
    forall(member(Spec, ['graph-cascade-menu.spec.ts',
                         'graph-object-type-picker.spec.ts']),
           member(Spec, Specs)).
test(recents_surface_registered) :-
    journeys_for_surface(documents_recents, ['documents-recents.spec.ts']).
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

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
