# Global agent policy

## Durable Prolog verification

For every task that changes files in the current work directory:

1. Use the `prolog-verification` skill.
2. Create or deliberately refresh `WORK_DIR/.prolog/facts.kb` and `WORK_DIR/.prolog/verify.pl` with `prolog-verify init --task <short-id>`. Use `--force` only when starting a new task and replacing old local verification state is intended.
3. Add task-specific requirements and invariants. Never assert `verified(true)`, `tests_passed`, or another self-certifying success fact.
4. Run real tests and checks through `prolog-verify observe -- <command> [args...]` so their argv, exit status, output hash, HEAD, and worktree digest are recorded by the machine.
5. Run `prolog-verify check` before claiming completion. Missing files, stale evidence, timeouts, exceptions, and non-zero exits are failures.

Keep `.prolog/` local unless the repository explicitly adopts its verifier as maintained project code. Do not weaken verification rules merely to make the gate pass.

## Brave-only external discovery

Use Brave Search for external discovery. Prefer `prolog-verify brave --query <query>` so the Brave call and result digest enter the verification facts together. A user-provided URL may be opened directly, but do not substitute another search engine or generic web-search tool for Brave.

Do not perform web research when the user forbids it. Local-only work does not need a search; keep `research_required(false)` for those tasks.
