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
