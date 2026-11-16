% OpenCode worker pipeline knowledge: deployment, model routing, and
% Unix-composability semantics for opencode-worker and its tests.
% Verified 2026-09-14 during the opencode-worker-pipes task
% (dotfiles feature/opencode-global-commands d0728f5, skills
% feature/opencode-command-skills f3b4ceb, live model smoke test).

% Canonical sources: the worker skill/scripts live in the skills flake
% (github lost-rob0t/skills, editable checkout
% /home/unseen/Documents/AI/skills); dotfiles opencode.org/opencode.nix
% only installs skills.packages.<system>.opencode-worker via
% home.packages. Never patch the wrapper in dotfiles.
owner('opencode-worker executable', 'skills flake inputs.skills').
deployment_path('opencode-worker', 'home.packages from inputs.skills packages; command at ~/.nix-profile/bin/opencode-worker').
activation_override('home-manager switch --flake .#unseen@desktop --override-input skills path:/home/unseen/Documents/AI/skills', 'deploys the editable skills checkout into the live profile').

% Model routing is data, not code: logical names resolve through the
% skill's config/models.tsv (logical<TAB>provider/model<TAB>variant) and
% the resolver probes the live `opencode models` catalog, failing with
% exit 3 when the mapped ID is unavailable.
model_mapping('astra-medium', 'openai/gpt-6-astra', 'medium').
resolver_probe('resolve-model', 'runs `opencode models` and rejects unavailable mappings with exit 3; OPENCODE_BIN and OPENCODE_WORKER_MODEL_CONFIG support testing').

% Linux execve caps ONE argv element near 128 KiB (MAX_ARG_STRLEN); the
% prompt is a single argv element of `opencode run`, so oversized prompts
% fail at exec time with E2BIG no matter the configured limit.
constraint(max_arg_strlen, 'prompt is one argv element; prompts at/above --max-prompt-bytes (default 131072) are rejected pre-launch with exit 2; raising the limit cannot succeed').

% GNU coreutils 9.11 cat splice()s a regular file into a pipe in ONE
% syscall (observed: splice(3->5)=200001 then splice(4->1)=200001), so a
% downstream consumer that exits after reading a few bytes never triggers
% EPIPE/SIGPIPE and `set -e` + cat reported success while data was lost.
% write(2)-chunked dd (bs=64K) restores observable closure (worker exit
% 141) and backpressure.
root_cause(worker_sigpipe_hidden,
    'coreutils 9.11 cat splice()s file-to-pipe atomically, hiding downstream closure; emit output via dd bs=64K so early consumer exit yields worker status 141').
unix_filter_contract('opencode-worker', 'stdin complete child task; stdout model result; stderr diagnostics; exit status preserved; downstream stdout closure terminates with 141').

% Test-harness gotchas proven while fixing the worker suite (Python 3.14):
% - Tool harnesses run children with SIGPIPE SIG_IGN; subprocess restore_signals
%   resets it for direct children, so a unittest parent still observes 141.
% - Environment values must stay far below 128 KiB: a single 200000-char env
%   var makes posix_spawn raise E2BIG ("Argument list too long") in Popen.
% - assert helpers reading a call log must tolerate the log never being
%   created when the worker correctly refuses to launch.
testing_pattern('fake opencode', 'OPENCODE_BIN points at a fake recording argv to a jsonl log; FAKE_* env vars script models/retries/sleep/output-size').
testing_gotcha(giant_env_var_e2big, 'keep fake-opencode output out of env vars; pass sizes and build data inside the fake to avoid E2BIG in posix_spawn').

% Smoke evidence (2026-09-14): live `printf | opencode-worker --model
% opencode/ling-3.0-flash-fin-free --role reviewer` returned the model
% answer on stdout with exit 0; piped grep matched; 200000-byte prompt
% rejected pre-launch with --max-prompt-bytes guidance.
smoke_verified('pipe end-to-end', 'stdin prompt -> free live model -> stdout answer, exit 0').
smoke_verified('composable downstream', 'worker stdout piped into grep matched expected answer').
smoke_verified('oversize guard', '200070-byte prompt rejected before launch with actionable guidance').
