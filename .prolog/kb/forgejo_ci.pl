% Forgejo CI provider knowledge for merge-on-green decisions.
% Verified 2026-09-14 during feature/opencode-global-commands (PR #204).

% The `Nix / workflows` job (mirrored .github/workflows/nix.yml) has failed
% on EVERY Forgejo commit in history, including master, always in "Set up
% job" (~9s) before actions/checkout runs; the job never executes repository
% code. This is a runner/label infrastructure failure, not a content gate.
% Do not block merges on it; do not expect it to go green on this provider.
chronic_infra_failure('forgejo-actions/nix-workflows',
    'job fails in Set up job before checkout on every commit including master; not a content gate').
mergeable_gate('forgejo-actions', 'contract').

% The runnable Forgejo jobs (e.g. zara-workflows.yml contract) succeed
% normally; treat those as the live Forgejo CI signal.
% The full nix.yml workflow remains authoritative on GitHub and can be
% replicated locally with the checks recorded in .prolog runs.
