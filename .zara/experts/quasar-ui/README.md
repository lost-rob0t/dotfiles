# quasar-ui — Quasar UI bug-hunting expert

Symptom → hypothesis → probe → verification rules over the desktop Quasar
UI surface, plus a deterministic corpus builder that indexes the operator
journey specs of a quasar checkout.

## Contents

- `kb/expert.pl` — `dotfiles_quasar_ui_expert`: seven authored chains
  (menu close-guard unexemption, duplicate Escape ownership,
  deleted-state setter references, cross-scope state declarations,
  e2e-dark CI gates, class-rename selector drift, post-deploy view-index
  rebuilds) plus the surface → e2e spec registry used as the verification
  gate.
- `corpus/build_corpus.pl` — `quasar_ui_corpus`: parses
  `<checkout>/frontend/e2e/*.spec.ts` (leading block comment = journey
  description, `test("...")` titles = journeys) into `journey/3` facts.
  Read-only against the checkout path you pass; no facts are copied into
  this pack.
- `tests/expert-tests.pl` — PlUnit suite, 17 tests. **Runs with no model
  and no network**: chain firing and non-firing, cross-chain false
  positives, surface registry shape, parser fixtures, and a corpus build
  over a temp fixture tree.

## Usage

```bash
# tests (the gate; exit 0 required)
swipl -q -t halt -g "run_tests" .zara/experts/quasar-ui/tests/expert-tests.pl

# corpus over a quasar checkout (read-only)
swipl -q -s .zara/experts/quasar-ui/corpus/build_corpus.pl \
  -g "load_corpus('/path/to/quasar'), forall(journey(S,_,T), writeln(S)), halt"
```

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
