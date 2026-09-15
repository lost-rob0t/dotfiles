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

% Formatting check invocation quirks (both verified 2026-09-14):
%   nix fmt -- --check (bare) passes no file args, nixfmt reads empty stdin
%   and dies with "unexpected end of input".
%   nix run .#formatter fails with "attribute 'formatter.type' does not exist".
nixfmt_check_command(['nix', 'fmt', '--', '--check', 'FILE...']).
nixfmt_invocation_gotcha(bare_check_reads_stdin,
    'nix fmt -- --check with no file args feeds empty stdin to nixfmt; always pass explicit files after --').
nixfmt_invocation_gotcha(nix_run_formatter_fails,
    'nix run .#formatter errors with "attribute formatter.type does not exist"; use nix fmt -- instead').
