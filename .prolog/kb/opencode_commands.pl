% Global OpenCode command system architecture (verified 2026-09-13).

% Four layers; strict dependency direction downward.
layer(slash_commands).
layer(skills).
layer(scripts).
layer(workers).
depends_on(skills, slash_commands).
depends_on(scripts, skills).
depends_on(workers, skills).

% Canonical sources.
literate_source('nix/home-manager/mods/opencode.org', 'nix/home-manager/mods/opencode.nix').
generated_commands_dir('~/.config/opencode/commands').
global_skills_checkout_env('OPENCODE_GLOBAL_SKILLS_CHECKOUT').
global_skills_checkout_default('~/Documents/AI/skills').

% Worker contract: opencode-worker from the skills repo, installed on
% PATH as packages.<system>.opencode-worker (flake output of the
% skills input).
worker_contract(stdin_task, stdout_result, stderr_diagnostics, preserved_exit_status).
worker_hard_rule('model/version/variant/agent-selected children MUST launch through opencode-worker').
worker_model_map('astra-medium', 'openai/gpt-6-astra', medium).
model_resolution_probes('opencode models').

% Hosting workflows.
provider_for('github.com', gh).
provider_for('git.starintel.actor', forgejo_api).
merge_on_green_invariant('merged commit must be the exact verified head; head change invalidates green evidence').
merge_on_green_exit(0, merged).
merge_on_green_exit(2, configuration).
merge_on_green_exit(3, provider_failure).
merge_on_green_exit(4, blocked_or_timeout).
merge_on_green_exit(5, rate_limited).

% Verification gates for the command system.
command_check('nix build .#checks.x86_64-linux.opencode-commands').
command_check('bash scripts/check-literate-sync').
command_name_regex('^[A-Za-z0-9][A-Za-z0-9._-]*$').

% Skills-repo verification entry point: hermetic pytest suite.
skills_repo_tests('python3 -m pytest tests/ -q').
