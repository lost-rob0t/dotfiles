% prolog-verify gate conventions for this repository.
% Source: PR 5b verification run (2026-09, agent/emacs-llm-overhaul-pr5b).

% Observation facts in runs/run-<HEAD>.pl wrap argv in command/1:
%   observation(Id, command(ArgList), exit(Code), OutputDigest, Head, WorktreeDigest)
verify_observation_schema(observation(id, command(argv), exit(code),
                                      output_digest, head, worktree_digest)).

% Worktree digest inputs: `git diff --binary HEAD` over tracked files
% (minus runtime .prolog state) plus hashed untracked non-ignored files.
% A fully committed, clean tree therefore hashes the empty string:
% sha256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855.
verify_clean_tree_digest('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855').

% The gate runs verify.pl with cwd = <repo>/.prolog, and check rewrites
% facts.kb with the freshly computed HEAD + digest.  Invariants that
% demand evidence "at current HEAD and digest" go stale on every new
% commit: re-run prolog-verify observe for each proof command AFTER the
% final commit, then run prolog-verify check.
verify_staleness_rule(observations_keyed_by(head, worktree_digest)).
verify_workflow(final_commit, observe_all, check).

% verify.pl runs from .prolog/, so repo-root-relative paths must be
% resolved via prolog_load_context(directory) + parent directory.
verify_path_convention(resolve_repo_root_from_load_context_parent).

% .prolog/kb/** is the tracked part of .prolog (whitelisted in
% .gitignore); runs/, facts.kb, verify.pl, result.json stay untracked.
verify_tracked_state(kb_only).
