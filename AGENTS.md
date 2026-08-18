# AGENTS.md

## Source of truth

This repository uses literate Org configuration. Treat the Org files as source code and tangled files as generated artifacts.

- `bash.org` is the source of truth for `.bashrc`.
- `.config/qtile/qtile-ai.org` is the main source of truth for `.config/qtile/config.py`.
- `.config/qtile/qtile-openrouter.org` is the source of truth for `.config/qtile/qtile_openrouter.py`.
- Do not make a lasting change only in a generated/tangled file.

When a change touches a literate configuration:

1. Edit the relevant Org source first.
2. Tangle it and commit the generated output in the same change.
3. Verify the generated output is in sync with its Org source.
4. Run syntax checks and the relevant tests before merging.

If an existing generated file has no literate source but belongs to a literate subsystem, add or identify the source before extending it. Do not create parallel sources of truth.

## Qtile

Keep keybindings documented in their literate source. The OpenRouter telemetry helper owns the dynamic `Super+Ctrl+Shift+R` sync-and-reload binding and must stay synchronized with `qtile-openrouter.org`.

Qtile sync/reload behavior must:

- use the shared `git-sync` implementation;
- never block Qtile's event loop while running Git;
- reload only after a successful sync;
- surface start, success, and failure through desktop notifications;
- retain the existing 1 Hz OpenRouter telemetry behavior unless intentionally changed.

## Bash

Interactive Bash must expose `git-sync` through the `.bashrc` generated from `bash.org`. The implementation lives in `.config/bash/git-sync.sh`; source that helper rather than duplicating its function body.

## Testing

Follow TDD for behavior changes. Add or update regression tests before implementation when practical, then run the real suite. Tests must validate literate/generated parity for files touched by a change rather than merely checking that both files exist.
