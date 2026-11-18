% agent_rules.pl -- the general repository editing contract, as facts.
% Migrated from AGENTS.md (2026-11-16): AGENTS.md is a thin loader; this
% file is the policy. Query rule families, don't re-read prose.

:- discontiguous([doc_rule/1, doc_location/2, cleanup_rule/1, testing_rule/1,
                  git_sync_rule/1, deploy_source/2, tasks_org_contract/1,
                  emacs_rule/1, script_rule/1, platform_rule/1]).

% ---- Documentation is part of the change ----
doc_rule(documentation_drift_is_a_blocking_consistency_error).
doc_rule(update_docs_same_branch_same_change_when_documented_behavior_changes).
doc_rule(architecture_ownership_deployment_interface_workflow_host_or_pair_changes_require_doc_updates).
doc_location('README.org', repository_entry_points_and_high_level_setup).
doc_location('docs/wiki/', architecture_ownership_workflows_maintenance_and_cross_subsystem_navigation).
doc_location(literate_org_sources, configuration_specific_rationale_keybindings_and_behavior).
doc_rule(link_canonical_source_instead_of_copying_configuration_details_into_the_wiki).
doc_rule(update_wiki_index_and_affected_pages_when_adding_or_renaming_major_subsystems_or_sources).
doc_rule(a_change_is_not_complete_while_implementation_and_relevant_docs_disagree).

% ---- Code cleanup ----
cleanup_rule(remove_dead_code_unreachable_branches_obsolete_compatibility_paths_and_unused_helpers_in_scope).
cleanup_rule(no_redundant_code_paths_without_a_concrete_current_requirement).
cleanup_rule(de_slopify_touched_code_remove_generated_looking_clutter_needless_comments_and_repetition).
cleanup_rule(prefer_the_smallest_clear_implementation_with_one_canonical_path).

% ---- Testing ----
testing_rule(follow_tdd_for_behavior_changes).
testing_rule(add_or_update_regression_tests_before_implementation_when_practical).
testing_rule(run_the_real_suite_not_a_stub).
testing_rule(tests_must_validate_literate_generated_parity_for_touched_files_not_mere_existence).
testing_rule(shared_shell_helpers_need_both_sourced_function_and_direct_command_coverage).
testing_rule(home_manager_evaluation_must_stay_green_for_changes_to_installed_commands).

% ---- Bash and git-sync ----
git_sync_rule(bash_org_sources_the_bashrc_and_must_source_the_shared_git_sync_sh_helper).
git_sync_rule(do_not_rely_on_bash_startup_files_alone_git_sync_must_be_directly_executable).
git_sync_rule(home_manager_installs_a_real_git_sync_command_built_from_the_same_helper).
git_sync_rule(do_not_make_home_manager_take_ownership_of_the_stow_managed_helper_path).

% ---- Deployment ownership ----
% Keep Codex/OpenCode clients synchronized instead of diverging copies.
deploy_source('nix/home-manager/files/global-agents.md',
    'user-global coding-agent contract deployed through the Codex and OpenCode Home Manager modules').
deploy_source('nix/home-manager/mods/agent-verification.nix',
    'durable Prolog gate packaging and Codex lifecycle hooks').
tasks_org_contract('tasks.org is the human personal Org command file (babel blocks, gptel helpers) invoked from Emacs; it is never agent task state or a work queue; agents may append tested invokable blocks following existing style and never delete or reorder existing content; the contract is global and applies to every repository with a tasks.org').

% ---- Emacs / Doom house rules (from CLAUDE.md, promoted to KB) ----
emacs_rule(doom_config_lives_in_doom_d).
emacs_rule(android_emacs_lives_in_android_as_literate_config).
emacs_rule(prefer_literate_org_that_tangles_to_configs_over_bare_config_files).
emacs_rule(follow_doom_standards_when_writing_vanilla_emacs_lisp).
emacs_rule(doom_emacs_lisp_goes_in_lisp_libraries_and_packages_not_config).
emacs_rule(non_config_elisp_goes_in_lisp_elisp).

% ---- Scripts and platform ----
script_rule(nixify_new_scripts_when_possible).
platform_rule(current_host_is_a_desktop_running_nixos_with_flakes).
