% Literate sync verification and Nix formatting knowledge.
% Verified 2026-09-14 during feature/opencode-global-commands (04a3acc).

% scripts/check-literate-sync tangles inside a throwaway worktree at HEAD
% and verifies only committed outputs. A dirty worktree only produces a
% WARNING; uncommitted (staged or untracked) Org sources are never checked.
% Symptom seen twice: a staged new Org/Nix pair "passed" vacuously at the
% old HEAD, then failed for real immediately after the commit.
vacuous_pass_risk('scripts/check-literate-sync',
    'new Org sources must be committed before check-literate-sync can verify them; staged-only pairs pass vacuously at the old HEAD').

% Tangle output is compared byte-for-byte with the committed generated file,
% so the Org src block text must be the nixfmt-rfc-style canonical form.
% Writing pretty-but-noncanonical Nix (e.g. compact short lists) in the Org
% block guarantees drift even when both files are individually valid.
invariant(org_src_block_text,
    'Nix src blocks in literate Org must contain nixfmt-rfc-style canonical text; raw tangle output must byte-match the committed file').
repair_recipe(literate_drift,
    'nixfmt the tangled output, paste the canonical text back into the Org src block, re-tangle, confirm git status shows only the Org modified').

% Formatting check invocation quirks (both verified 2026-11-14):
%   nix fmt -- --check (bare) passes no file args, nixfmt reads empty stdin
%   and dies with "unexpected end of input".
%   nix run .#formatter fails with "attribute 'formatter.type' does not exist".
nixfmt_check_command(['nix', 'fmt', '--', '--check', 'FILE...']).
nixfmt_invocation_gotcha(bare_check_reads_stdin,
    'nix fmt -- --check with no file args feeds empty stdin to nixfmt; always pass explicit files after --').
nixfmt_invocation_gotcha(nix_run_formatter_fails,
    'nix run .#formatter errors with "attribute formatter.type does not exist"; use nix fmt -- instead').

% ---- Authoritative Org -> generated pair registry ----
% This registry moved out of AGENTS.md (2026-11-16): the checker discovers
% pairs dynamically from :tangle declarations; these facts give agents a
% queryable ownership map.  Register every NEW pair here in the same change.
literate_pair('bash.org', '.bashrc').
literate_pair('.config/qtile/qtile-ai.org', '.config/qtile/config.py').
literate_pair('.config/qtile/qtile-ai-windows.org', '.config/qtile/qtile_ai_windows.py').
literate_pair('.config/qtile/qtile-workflows.org', '.config/qtile/qtile_workflows.py').
literate_pair('.config/qtile/qtile-workflows.org', '.config/qtile/workflows.json').
literate_pair('.config/qtile/qtile-capture.org', '.config/qtile/qtile_capture.py').
literate_pair('.config/qtile/qtile-prompt-lib.org', '.config/qtile/qtile_prompt_lib.py').
literate_pair('.config/qtile/qtile-openrouter.org', '.config/qtile/qtile_openrouter.py').
literate_pair('.doom.d/autoload/gpt-todos.org', '.doom.d/autoload/gpt-todos.el').
literate_pair('.doom.d/autoload/distro.org', '.doom.d/autoload/distro.el').
literate_pair('scripts/gpt-todos-sync.org', 'scripts/gpt-todos-sync').
literate_pair('scripts/gpt-todos-sync.org', 'scripts/install-gpt-todos-cron').
literate_pair('scripts/dotfiles-sync.org', 'scripts/dotfiles-sync').
literate_pair('scripts/distro.org', 'scripts/distro').
literate_pair('scripts/termux-remote.org', 'scripts/remote-gui-launch').
literate_pair('scripts/termux-remote.org', 'scripts/agent-zero-tunnel').
literate_pair('scripts/termux-remote.org', 'scripts/install-termux-widgets').
literate_pair('scripts/termux-starintel-client.org', 'scripts/termux-starintel-client').
literate_pair('scripts/termux-couchdb-proof.org', 'scripts/termux-couchdb-proof').
literate_pair('scripts/termux-starintel.org', 'scripts/termux-starintel').
literate_pair('bootstrap-termux.org', 'bootstrap-termux.sh').
literate_pair('android/build.org', 'android/build.py').
literate_pair('android/early-init.org', 'android/early-init.el').
literate_pair('android/config.org', 'android/lisp/star-android-modern.el').
literate_pair('android/config.org', 'android/stow/android-emacs').
literate_pair('android/config.org', 'android/bin/install-native-profile').
literate_pair('android/config.org', 'android/tests/star-android-modern-test.el').
literate_pair('nix/home-manager/mods/opencode.org', 'nix/home-manager/mods/opencode.nix').
literate_pair('nix/home-manager/mods/starintel-llm-harness.org', 'nix/home-manager/mods/starintel-llm-harness.nix').
literate_pair('scripts/opencode.org', '.local/bin/opencode-tmux').
literate_pair('scripts/opencode.org', '.local/bin/opencode-pick').
literate_pair('scripts/opencode.org', '.local/bin/ezf').
literate_pair('scripts/opencode.org', '.local/bin/ezf.sh').
literate_pair('scripts/opencode.org', '.local/bin/opencode-ses').
literate_pair('scripts/opencode.org', '.local/lib/dotfiles/opencode_tmux.py').
literate_pair('scripts/opencode.org', '.local/lib/dotfiles/opencode_ses.py').
literate_pair('scripts/opencode.org', '.local/lib/dotfiles/ezf.el').
literate_pair('scripts/opencode.org', 'nix/home-manager/mods/opencode-tmux.nix').
literate_pair('scripts/opencode.org', 'nix/home-manager/mods/opencode-ses.nix').
literate_pair('android/doom/config.org', 'android/doom/init.el').
literate_pair('android/doom/config.org', 'android/doom/packages.el').
literate_pair('android/doom/config.org', 'android/doom/config.el').
literate_pair('emacs/profiles.org', '.emacs-profiles.el').

% ---- Literate change procedure (the rules agents follow) ----
literate_change_step(1, edit_the_relevant_org_source_first).
literate_change_step(2, tangle_and_commit_generated_output_in_same_change).
literate_change_step(3, verify_generated_output_is_in_sync_with_org_source).
literate_change_step(4, run_syntax_checks_and_relevant_tests_before_merge).
literate_rule(new_generated_file_without_source_needs_a_source_before_extension).
literate_rule(never_create_parallel_sources_of_truth).
literate_gate(check_literate_sync_green_before_claiming_completion).
