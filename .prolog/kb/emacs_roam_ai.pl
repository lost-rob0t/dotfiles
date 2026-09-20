% Roam AI / emacs-llm-overhaul facts: rights gate, sidebar chat, batch-safe
% module conventions, and the stacked PR series state.
% Source: PR series agent/emacs-llm-overhaul-pr1..pr4 (lisp/llm/ai-roam*.el).

% Rights gate resolution order (ai-roam.el).
roam_rights_order([global_toggle(ai/full-editor_rights),
                   per_section(ai/roam-section-rights),
                   default(ai/roam-default-rights)]).
roam_section_rights_default(full, [hacking, scada, writing, meta]).
roam_section_rights_default(outline, remaining_sections).
roam_gptel_tool(create_roam_id_link, ai_roam_links, rights_gated(full)).
roam_gptel_tool(search_notes_semantic, ai_roam_vector, read_only).
roam_gptel_tool(index_notes_embeddings, ai_roam_vector, read_only).
roam_gptel_tool(remember_fact, ai_roam_memory, rights_gated(full)).
roam_gptel_tool(recall_memory, ai_roam_memory, read_only).
roam_gptel_tool(assert_world_fact, ai_roam_memory, rights_gated(full)).

% Layered memory (ai-roam-memory.el, PR 5).
memory_layer(1, roam_notes, ai_roam_memory,
             note_dir("llm/memory/"), relative_to(ai/roam-directory)).
memory_layer(2, world_facts, ai_roam_memory,
             kb_file("llm/memory/kb-facts.pl"), append_only).
world_fact_line_schema([uuid, quoted(subject), quoted(predicate),
                        quoted(object), source_or_none, agent,
                        weight(1), active, iso8601_utc]).
memory_write_guarantees(stable_org_id_across_rewrites,
                        reads_existing_top_drawer_id_first).
roam_regex_pitfall(unescaped_plus_after_anchor,
                   "regex \\'`#+TITLE silently never matches: #+TITLE in a regex needs \\'`#\\+TITLE (the + is a quantifier)").

% Doom-free llm modules must stay batch-safe: no hard gptel/ai/org-roam
% require at top; guard with (require 'x nil t) and degrade gracefully.
batch_safe_pattern(llm_module, require_soft(gptel)).
batch_safe_pattern(llm_module, declare_function(ai/llm_backend, ai)).
batch_safe_pattern(llm_module, declare_function(ai/llm_resolve_model, ai)).
batch_safe_pattern(llm_module, org_roam_backlinks_omitted_in_batch).
batch_safe_pitfall(string_match_p_respects_case_fold_search,
                   "batch case-fold-search=t: an uppercase marker test (FULL) also matches lowercase prose; keep markers unique case-insensitively").

% Tool registration wiring (ai-init.el).
wired_at_startup(ai/image_register_gptel_tools, hard_module_require).
wired_at_startup(ai/prolog_rlm_register_gptel_tool, hard_module_require).
wired_at_startup(ai/roam_links_register_gptel_tools, ignore_errors_call_site).
wired_at_startup(ai/roam_vector_register_gptel_tools, ignore_errors_call_site).
wired_at_startup(ai/roam_memory_register_gptel_tools, ignore_errors_call_site).
wiring_note("roam register functions user-error without gptel, unlike image/prolog whose modules hard-require gptel at load; ai-init call sites are wrapped in ignore-errors").

% Sidebar chat (ai-roam-chat.el).
roam_sidebar_chat_buffer_name("*roam: SECTION*", explicit_name_wins).
roam_sidebar_chat_display(side_window(right), width_fraction(0.35), dedicated(t)).
roam_sidebar_chat_system_message_var(gptel_system_message).
roam_section_wording_marker(nil_section, "no specific roam section").
roam_rights_wording_marker(full, "FULL editor rights").
roam_rights_wording_marker(outline, "Rights are OUTLINE-ONLY").

% Named gptel profiles (ai-roam-profiles.el, PR 5b).
roam_profile(roam, system_fn(ai/roam-chat--system-message), tools_fn(ai/roam-chat--tools)).
roam_profile(roam_outline, system_fn(nil_outline_message), tools_fn(nil)).
profile_preset_name(Preset) :- atom_concat('roam:', Profile, Preset), atom(Profile).
profile_apply_invariants([preset_defined_once_via_hash,
                          gptel_preset_set_when_boundp,
                          buffer_local_system_and_tools_always_set,
                          direct_setq_is_authoritative]).
profile_send_integration(:before_advice(gptel_send),
                         scan_scope(line_begin_to_point),
                         deletes_mention_then_applies,
                         advice_added_once_global_flag).
profile_setup(idempotent, silent_noop_without_gptel, wired_ignore_errors(ai_init)).

% Stacked series state (planning run: run-6cf097a-emacs-llm-overhaul.pl).
llm_overhaul_pr(1, ai_roam_foundation, done).
llm_overhaul_pr(2, roam_id_repair, done).
llm_overhaul_pr(3, org_vector_tools, done).
llm_overhaul_pr(4, roam_sidebar_chat, done).
llm_overhaul_pr(5, layered_memory, done).
llm_overhaul_pr('5b', gptel_roam_profiles, done).
llm_overhaul_pr(6, roam_rewrite_system, pending).
llm_overhaul_pr(7, writing_coach, pending).
llm_overhaul_pr(8, publish_censor_private, pending).
llm_overhaul_pr(9, roam_kb_prolog, pending).
llm_overhaul_pr(10, launcher_scripts_docs, pending).
