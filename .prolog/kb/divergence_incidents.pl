%% Divergence/duplication incidents in the dotfiles repo and their
%% recovery patterns. Verified 2026-11-15 rescue of the 2026-11-04..15
%% master divergence (backup ref: backup/diverged-master-20261115).

%% Incident: local master diverged (ahead 7 / behind 42) because the same
%% work landed on origin through PRs from another machine while this
%% checkout kept unpushed twins. git cherry upstream=head patch-id equality
%% identified 5 of 7 ahead commits as duplicates; rebase
%% --onto origin/master <merge-base> dropped them automatically
%% ("patch contents already upstream").
divergence_rescue(2026_11_15,
    method('git rebase --onto origin/master b73d7742 master',
           'skip patch-identical twins, resolve real conflicts, keep local-only work')).

%% The only truly local-only casualties of a divergence are (a) unpushed
%% commits with no origin twin and (b) uncommitted working-tree changes.
%% In this incident the operator-visible symptom was zara-dev missing from
%% PATH: the re-enable lived only in dirty files, hm generations 157..160
%% were built from the disabled config, and zara-workflows.nix gates the
%% toggle behind zara.enable/workflows.enable.
divergence_symptom_rule(missing_command,
    'command not found after an hm switch means the enabling commit/config never reached the built profile; check git status dirty files and generation bins before assuming data loss').

%% Forgejo vs GitHub mirror state (observed 2026-11-15): github/master
%% strictly contained forgejo origin/master (0 behind, 9 ahead: PR #331
%% qtile focus, PR #361 LLM Luna). The Forgejo push quirk had left the
%% primary stale; the rescue rebased local commits onto the GitHub
%% superset. Re-resolve remotes before any future ff-only pull from Forgejo.
divergence_resolution(two_forges_2026_11_15, rebase_onto_github_superset,
    'forgejo stale behind github; local rescue commits replayed onto github/master, forgejo sync deferred until its push quirk is fixed').

%% Conflict resolutions used in the 2026-11-15 rescue, reusable as precedent:
divergence_resolution(prolog_mcp, take_origin,
    'origin ad5f787d constructor-injection port supersedes the local mcp-2.x fix; keep origin file, keep local kb additions').
divergence_resolution(zara_workflows_test, take_local,
    'local enabled-state contract supersedes origin disabled-era assertions; local version retains all unrelated origin assertions').
divergence_resolution(wiki_module_table, union,
    'keep origin opencode/opencode-security rows, keep local zaiCodingPlan codex row').
