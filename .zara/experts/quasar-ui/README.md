# quasar-ui — Quasar UI bug-hunting expert

Symptom → hypothesis → probe → verification rules over the desktop Quasar
UI surface, plus a deterministic corpus builder that indexes the operator
journey specs of a quasar checkout.

## Contents

- `kb/expert.pl` — `dotfiles_quasar_ui_expert`: authored chains
  (menu close-guard unexemption, duplicate Escape ownership,
  deleted-state setter references, cross-scope state declarations,
  e2e-dark CI gates, class-rename selector drift, post-deploy view-index
  rebuilds) plus the FULL surface inventory: every app route
  (`surface_route/2`), every tracked journey spec (`surface_spec/2`,
  25 files), and declared gaps (`surface_uncovered/2`).
- `kb/run_ledger.pl` — `quasar_ui_run_ledger`: reads the quasar
  checkout's run ledger (`<checkout>/.prolog/runs/run-<HEAD>.pl`,
  written by `scripts/record-run.mjs`) through an explicit-path
  adapter. Queries: `latest_result/3` (rerun-superseding),
  `commit_health/2` (green/red), `commit_failures/2`,
  `first_failing_commit/3` (regression entry point over an ordered
  git-log commit list). This is the self-updating half of the KB: every
  test run in CI or locally extends the facts; nothing here writes.
- `corpus/build_corpus.pl` — `quasar_ui_corpus`: parses
  `<checkout>/frontend/e2e/*.spec.ts` (leading block comment = journey
  description, `test("...")` titles = journeys) into `journey/3` facts,
  and audits coverage: `unmapped_specs/3` reports spec files on disk
  that the surface registry does not map. Read-only against the
  checkout path you pass; no facts are copied into this pack.
- `tests/expert-tests.pl` — PlUnit suite, 21 tests. **Runs with no model
  and no network**: chain firing and non-firing, cross-chain false
  positives, the full surface registry, parser fixtures, corpus build
  over a temp fixture tree, and the run ledger (health, rerun
  supersession, failure extraction, regression bisection).

## Usage

All invocation goes through the single interface (see
`.zara/experts/AGENTS.md`):

```bash
# tests (the gate; exit 0 required)
.zara/experts/bin/zara-expert test quasar-ui

# symptom -> hypothesis -> probe -> verification
.zara/experts/bin/zara-expert diagnose surface=graph item_press_unmounts_menu=true

# coverage audit: every tracked journey spec must be mapped
.zara/experts/bin/zara-expert coverage --checkout /path/to/quasar

# per-commit suite health from the run ledger
.zara/experts/bin/zara-expert health --ledger /path/to/quasar/.prolog/runs \
  --commits c1,c2,c3

# scaffold a new expert pack when no pack fits the domain
.zara/experts/bin/zara-expert define <name> <kind>

# raw suite invocation (what the interface wraps)
swipl -q -t halt -g "run_tests" .zara/experts/quasar-ui/tests/expert-tests.pl

# corpus + coverage audit over a quasar checkout (read-only)
swipl -q -s .zara/experts/quasar-ui/corpus/build_corpus.pl \
  -g "use_module('.zara/experts/quasar-ui/kb/expert'), \
      findall(S, surface_spec(_, S), Mapped), \
      unmapped_specs('/path/to/quasar', Mapped, Unmapped), \
      writeln(Unmapped), halt"
```

Record runs (quasar side, local or CI):

```bash
cd quasar
npm run record:vitest
npm run record:e2e -- e2e/graph-cascade-menu.spec.ts
```

CI (quasar `.github/workflows/ci.yml`): the `validate` job runs the
bounded journey subset on every Forgejo PR (chromium + the three
operator journeys) and uploads the ledger as an artifact; the full
chromium chain stays GitHub-only (Forgejo box cannot finish it - runs
72/73/74).

Expert use: `observe/2` the symptom facts, enumerate `hypothesis/1`
(exactly one should fire for a clean symptom set), act on
`next_probe/2`, and treat the paired `verification/2` as the evidence
bar before claiming a fix.

## Provenance and ownership

- Definitions owned here (dotfiles `.zara/experts/`); the canonical
  `ZARA-EXPERT/1` descriptor runtime (`~/starintel/zara-expert`,
  milestones E0–E4) is not implemented yet — runtime registration will
  consume the canonical descriptor adapter once it exists. No local
  replacement manifest is fabricated here.
- Incident knowledge distilled from the quasar repository's durable KB
  (`.prolog/kb/ui_e2e_harness.pl`, `.prolog/kb/testing-pitfalls.pl`) and
  live operator incidents: forgejo `nsaspy/quasar` PRs #147/#148/#149
  (2026-11-20). The corpus builder reads the quasar checkout live —
  project facts stay in the project KB; nothing is duplicated here.
