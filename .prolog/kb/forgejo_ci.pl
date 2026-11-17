% Forgejo CI workflow facts verified while repairing PR #243 on 2026-10-24.

workflow_gate('Nix / workflows', '.github/workflows/nix.yml').
workflow_linter('Nix / workflows', actionlint, 'v1.7.12').
workflow_linter_digest('v1.7.12', '8aca8db96f1b94770f1b0d72b6dddcb1ebb8123cb3712530b08cc387b349a3d8').

% The runner job container has no /var/run/docker.sock. Docker-based
% devops-actions/actionlint failed after checkout; the pinned binary runs
% directly in that container and preserves the lint gate.
runner_constraint('Nix / workflows', no_docker_socket).
workflow_fix('Nix / workflows', pinned_actionlint_binary).

% The logos Home Manager gate forces these pinned sources. Public Forgejo
% URLs let an unauthenticated runner fetch the exact commits.
ci_source_fetch('Nix / logos', quasar, public_forgejo).
ci_source_fetch('Nix / logos', star_lang, public_forgejo).

% Nix logos setup fails before checkout when Cachix resolves against the
% runner default data.forgejo.org; that host has no Cachix repositories.
workflow_fix('Nix / logos', fetch_versioned_upstream_cachix_as_local_actions).

workflow_failure('Nix / logos', monolithic_flake_evaluation_exceeds_2_gib_container_cap).
workflow_fix('Nix / logos', evaluate_each_check_and_package_in_separate_process).

% Facts verified while repairing PR #353 on 2026-11-16.
%
% data.forgejo.org redirects cachix/* to code.forgejo.org, which no longer
% hosts install-nix-action or cachix-action; every step using the short
% cachix/* names failed at action clone time. Fix: clone from github.com into
% .github/ci-actions/ and use local uses: paths (applied to llm-log-qtile,
% gptel-tool-results, main, prolog-rlm-tool, iso, update-flake in PR #353).
workflow_failure(all_workflows, cachix_actions_missing_from_data_forgejo_org).
workflow_fix(all_workflows, clone_cachix_actions_from_github_as_local_actions).

% The runner rewrites actions/checkout fetches of external repositories
% through its internal git proxy http://172.30.0.1:3100/<owner>/<repo>,
% which 404s repos not present in its cache (karthink/gptel,
% lost-rob0t/prolog-rlm). Plain `git clone https://github.com/...` run steps
% are NOT rewritten and work; replace checkout-action external checkouts
% with a --no-checkout clone plus pinned-sha checkout.
workflow_failure(external_repo_checkouts, runner_git_proxy_404s_uncached_repos).
workflow_fix(external_repo_checkouts, plain_git_clone_run_step_with_pinned_sha).
