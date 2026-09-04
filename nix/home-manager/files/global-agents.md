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

## Durable research artifacts

For durable research, investigations, architectural analysis, source review, benchmark analysis, incident analysis, and similar work, default to Org rather than ephemeral chat-only prose.

1. Resolve the user's configured Org-roam root instead of hardcoding a home-directory path.
2. Write or update the artifact at `llm/research/<slug>.org` beneath that root. Use a stable lowercase filesystem-safe slug derived from the subject, and reuse an existing artifact for the same subject instead of creating near-duplicates.
3. Use normal Org structure and preserve sources, exact commands, commit SHAs, evidence, uncertainty, and conclusions. Prefer headings, links, tables, source blocks, TODOs, and properties over Markdown pasted into Org.
4. After the artifact is successfully written, use the `emacs-eval` skill to open that exact file in the user's running Emacs server.
5. Do not claim the file was opened unless the Emacs operation actually succeeded.

Do not create a durable research artifact for trivial scratch work unless the user asks for one.

## Code cleanup

- Remove dead code, unreachable branches, obsolete compatibility paths, and unused helpers encountered within the task scope.
- Do not preserve redundant code paths without a concrete current requirement.
- De-slopify touched code: remove generated-looking clutter, needless comments, repetition, unnecessary abstraction, and defensive complexity that does not serve an identified behavior.
- Prefer the smallest clear implementation with one canonical path while preserving required behavior and focused diffs.
