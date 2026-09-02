# AGENTS.md

## Read this first

Before editing this repository, read this file and treat it as the repository editing contract.

## Source of truth

This repository uses literate Org configuration. Treat the Org files as source code and tangled files as generated artifacts.

- `bash.org` is the source of truth for `.bashrc`.
- `.config/qtile/qtile-ai.org` is the main source of truth for `.config/qtile/config.py`.
- `.config/qtile/qtile-ai-windows.org` is the source of truth for `.config/qtile/qtile_ai_windows.py`.
- `.config/qtile/qtile-workflows.org` is the source of truth for `.config/qtile/qtile_workflows.py` and `.config/qtile/workflows.json`.
- `.config/qtile/qtile-capture.org` is the source of truth for `.config/qtile/qtile_capture.py`.
- `.config/qtile/qtile-openrouter.org` is the source of truth for `.config/qtile/qtile_openrouter.py`.
- `.doom.d/autoload/gpt-todos.org` is the source of truth for `.doom.d/autoload/gpt-todos.el`.
- `scripts/gpt-todos-sync.org` is the source of truth for `scripts/gpt-todos-sync` and `scripts/install-gpt-todos-cron`.
- `scripts/dotfiles-sync.org` is the source of truth for `scripts/dotfiles-sync`.
- `scripts/termux-remote.org` is the source of truth for `scripts/remote-gui-launch`, `scripts/agent-zero-tunnel`, and `scripts/install-termux-widgets`.
- `bootstrap-termux.org` is the source of truth for `bootstrap-termux.sh`.
- `android/doom/config.org` is the source of truth for `android/doom/init.el`, `android/doom/packages.el`, and `android/doom/config.el`.
- Do not make a lasting change only in a generated/tangled file.

When a change touches a literate configuration:

1. Edit the relevant Org source first.
2. Tangle it and commit the generated output in the same change.
3. Verify the generated output is in sync with its Org source.
4. Run syntax checks and the relevant tests before merging.

If an existing generated file has no literate source but belongs to a literate subsystem, add or identify the source before extending it. Do not create parallel sources of truth.

## Documentation is part of the change

Keep repository documentation synchronized with the implementation. Documentation drift is a blocking consistency error, just like literate/generated drift.

For every change, determine whether it affects documented behavior or repository structure. If it changes behavior, architecture, ownership, deployment, interfaces, workflows, host/profile names, commands, important invariants, or a canonical source/generated mapping, update the relevant documentation in the same branch and PR.

Documentation locations include:

- `README.org` for repository entry points and high-level setup;
- `docs/wiki/` for architecture, ownership, workflows, host/profile maps, maintenance procedures, and cross-subsystem navigation;
- the canonical literate Org source for configuration-specific rationale, keybindings, widgets, and behavior that belongs next to executable configuration;
- subsystem-specific docs when they already exist.

Do not create duplicate sources of truth by copying large configuration details into the wiki. Link to the canonical source and document the architecture, invariant, or workflow instead.

When adding or renaming a major subsystem, host/profile, canonical Org source, generated output, or deployment path, update `docs/wiki/index.org` and any affected wiki page so navigation remains accurate.

A change is not complete if the implementation and relevant documentation disagree.

## Qtile

Keep keybindings documented in their literate source. The OpenRouter telemetry helper owns the dynamic `Super+Ctrl+Shift+R` sync-and-reload binding and must stay synchronized with `qtile-openrouter.org`.

Qtile sync/reload behavior must:

- use the shared `git-sync` implementation;
- never block Qtile's event loop while running Git;
- reload only after a successful sync;
- surface start, success, and failure through desktop notifications;
- retain the existing 1 Hz OpenRouter telemetry behavior unless intentionally changed.

## Bash and git-sync

`bash.org` and its tangled `.bashrc` should source the shared `.config/bash/git-sync.sh` helper so interactive Bash gets a `git-sync` function when that Bash configuration is active.

Do not rely on Bash startup files as the only way to expose `git-sync`. The helper must also be directly executable, and the base Home Manager module must install it as a real `git-sync` command built from the same helper. Do not make Home Manager take ownership of the Stow-managed helper path merely to expose the command.

## GPT TODO sync

`lost-rob0t/gpt-todos` owns durable Org task state. Dotfiles owns the local sync/runtime integration used to move that state into the user's live Org agenda.

Do not put the canonical sync executable or Emacs integration in `gpt-todos`. Keep them in this repository and keep their Org sources synchronized with their generated counterparts.

`gpt-todos-sync` must only synchronize GPT TODO/agenda state. It must never tangle, stage, commit, fetch, rebase, push, or otherwise synchronize the dotfiles repository.

Dotfiles synchronization belongs exclusively to `dotfiles-sync`. `dotfiles-sync` may enforce known literate/generated pairs and must target the user's Forgejo service at `git.starintel.actor` by default rather than GitHub.

## Testing

Follow TDD for behavior changes. Add or update regression tests before implementation when practical, then run the real suite. Tests must validate literate/generated parity for files touched by a change rather than merely checking that both files exist.

For shared shell helpers, test both sourced-function behavior and direct command execution. Home Manager evaluation must remain green for changes to installed commands.
