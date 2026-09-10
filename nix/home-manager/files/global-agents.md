# Global agent policy
## Git operations
For all git operations we use a split setup
Main forgjo instance lives at git.starintel.actor
As well as github.com

Prefer using git.starintel.actor for everything
origin should live at that host.
if there is outage with that host then use github as fall back.

use tea for forgjo actions.

## Durable Prolog project memory

For substantive repository work, use the `prolog-project-kb` skill.

- Query the project's durable Prolog KB before starting work.
- Keep active TODO state, blockers, dependencies, and observations in `.prolog/runs/run-<HEAD>.pl`; update it immediately as work changes state.
- No substantive work without current Prolog TODO state, and do not leave TODO state stale after a meaningful transition.
- Keep `.prolog/runs/` local/untracked so execution bookkeeping does not dirty Git.
- Promote verified reusable discoveries into `.prolog/kb/`: architecture, tools, useful scripts/commands, debugging symptoms, root causes, fixes, invariants, dependencies, and recurring workflows.
- The durable KB must be composed from many focused Prolog files with one obvious loader. Do not impose a fixed file taxonomy; inspect the existing KB and discover/evolve the structure that best models the project.
- Prefer relations and queryable predicates over prose logs. Raw observations stay in `run-<HEAD>.pl`; durable KB files contain knowledge worth giving future agents.

## Durable Prolog verification

For every task that changes files in the current work directory:

1. Use the `prolog-verification` skill.
2. Create or deliberately refresh `WORK_DIR/.prolog/facts.kb` and `WORK_DIR/.prolog/verify.pl` with `prolog-verify init --task <short-id>`. Use `--force` only when starting a new task and replacing old local verification state is intended.
3. Add task-specific requirements and invariants. Never assert `verified(true)`, `tests_passed`, or another self-certifying success fact.
4. Run real tests and checks through `prolog-verify observe -- <command> [args...]` so their argv, exit status, output hash, HEAD, and worktree digest are recorded by the machine.
5. Run `prolog-verify check` before claiming completion. Missing files, stale evidence, timeouts, exceptions, and non-zero exits are failures.

Keep verifier-generated runtime state local. `.prolog/kb/` is the exception when the project uses the durable project-memory skill: verified reusable KB knowledge is intended to be tracked. Do not weaken verification rules merely to make the gate pass.

## Brave-only external discovery

Use Brave Search for external discovery. Prefer `prolog-verify brave --query <query>` so the Brave call and result digest enter the verification facts together. A user-provided URL may be opened directly, but do not substitute another search engine or generic web-search tool for Brave.

Do not perform web research when the user forbids it. Local-only work does not need a search; keep `research_required(false)` for those tasks.
