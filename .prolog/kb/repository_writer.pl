% Exact-path audited Org writes, verified by tests/kb-roam-repository.el.
repository_writer(entrypoint, 'scripts/kb-ingest --root ROOT --path RELATIVE.org --document-file DRAFT --source SOURCE --expected-sha256 HASH_OR_absent').
repository_writer(owner, 'lisp/wiki/kb-roam.el:kb-roam-write-document/4').
repository_writer(invariant, preserves_existing_file_id_and_ai_tag).
repository_writer(invariant, rejects_stale_updates_and_symlink_components).
repository_writer(invariant, requires_two_topical_tags_and_concrete_provenance).
repository_writer(boundary, cooperative_local_file_lock_and_atomic_rename_not_hostile_same_user_race_defense).
repository_writer(runtime_workaround, 'When the Emacs daemon is unresponsive, batch invocation of kb-roam-cli-write-document runs the same audited writer; do not mutate Org with a substitute script.').
