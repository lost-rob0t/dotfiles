# AGENTS.md

## Read this first

This file is a loader, not the policy. The repository rules are Prolog facts — consult them before and while working:

```bash
swipl -q -f .prolog/kb/index.pl -t halt   # or load via the prolog MCP in any harness
```

Where the rules live:

- **Literate sources/generated pairs** — `literate_pair(Org, Output)` and `literate_change_step/2` in `.prolog/kb/literate_sync.pl`. Verify with `scripts/check-literate-sync` (run it before claiming completion; register new pairs in the KB in the same change).
- **Editing contract** (docs, cleanup, testing, git-sync, Doom rules) — `.prolog/kb/agent_rules.pl`.
- **Qtile rules** — `.prolog/kb/qtile_rules.pl`.
- **GPT TODO sync** — `.prolog/kb/gpt_todos_sync.pl`. **Zara expert ownership** — `.prolog/kb/zara_experts_opencode.pl`.
- **Distro release engineering** (issues.org intake, council voting, gated releases) — `.prolog/kb/distro_release.pl` (see `distro_command/2` for the full CLI inventory: `feat`, `bug`, `triage`, `sync`, `status`, `next`, `claim`, `done`, `drop`, `dep`, `verify`, `vote`, `attack`, `release`) and `docs/wiki/distro-release-engineering.org`. The `distro` command is on PATH via HM `base.nix`; `distro help` works from anywhere.
- Supplemental plain-text gates: `ZAGENTS.md`.

Global agent policy (shared memory, Prolog TODO state, verification gates, Brave-only research) deploys from `nix/home-manager/files/global-agents.md` via the Codex and OpenCode Home Manager modules.
