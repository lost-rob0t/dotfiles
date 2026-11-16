---
name: distro-work
description: dotfiles meta-distro issue intake, claiming, implementation, evidence; use when working issues.org or scripts/distro in this repo
compatibility: Any agent harness with filesystem + shell access (OpenCode, Codex, Claude, pi, Zara)
---

# Distro work — implement tracked issues in the dotfiles meta-distro

The dotfiles repository is a meta-distro.  Humans file one-line feature
requests and bug reports; agents turn them into done, evidenced work.  The
contract is files + `scripts/distro` + SWI-Prolog — no harness-specific
APIs — so this skill applies to any agent in any harness.

## Orientation

1. Read `AGENTS.md` (literate sources, docs, cleanup rules all apply to
   distro work like anything else).
2. `distro status` — current version, milestones, release-gate verdict.
3. `distro next` — the highest-priority actionable issue.  Also query the
   engine directly in a live Prolog session when you need more than the
   report (see "Prolog substrate" below).

## Filing (when the human asks in chat)

`distro feat "one-liner"` / `distro bug "one-liner" --severity high`, or
append a `** FEAT:/BUG:` heading under `* Inbox` in `issues.org`, then
`distro triage`.  Never hand-write `ISSUE-nnnn` ids; only `distro triage`
mints them.  `tasks.org` is the human's own Emacs command file — it is NOT
the intake, never file issues there.

## Working an issue

1. `distro claim ISSUE-xxxx --agent <your-id>` — your agent id goes in
   `DISTRO_AGENT` or `--agent`; claim before starting so others skip it.
2. Implement following repository rules: literate Org sources first,
   tangle + commit generated output, docs updated, TDD.
3. Record evidence against the issue (this is what the release gate
   checks):
   `distro verify ISSUE-xxxx -- bash tests/distro-test.sh`
   Any real command works; the engine records exit status, output digest
   and the issues.org epoch it saw.
4. `distro done ISSUE-xxxx` only after evidence is green.
5. If the issue turns out wrong or won't-fix: `distro drop ISSUE-xxxx
   <reason>` — never silently delete entries.

## Dependencies

Real dependencies are declared facts, not vibes:
`distro dep ISSUE-a ISSUE-b` ("a needs b").  Compute dependencies when you
see them; declare them explicitly.  The engine blocks releases on unmet
deps.

## Prolog substrate (preferred over text scraping)

The engine is a live Prolog program — load it and query it directly:

```prolog
?- [distro/engine/engine].          % auto-loads distro/state/*
?- distro_root(R).                  % resolved checkout root
?- release_ready('0.2.0').
?- release_blocker('0.2.0', B).     % why not ready, one answer per reason
?- reload_distro_state.             % after any `distro` command changed state
```

From OpenCode use the `prolog` MCP tools (`create_session` with the engine
file, then `inspect_predicate`, `replace_predicate` only for genuine engine
fixes, `run_tests`).  Do not parse CLI text output when a predicate query
answers the question.

## Council and releases

Not in this skill.  Reviewing, voting, and releasing follow
`.opencode/skills/distro-council/SKILL.md`.  Do not run `distro release`
from work context.
