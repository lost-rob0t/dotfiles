# Auto Research expert

This is the canonical dotfiles-owned Prolog expert and deterministic corpus
builder for `starintel-auto-research`. The repository owns its mirrored project
knowledge under `.prolog/kb/`; this expert consumes it without copying its facts.
No registry descriptor or Zara activation is implied. Python 3.10+, Git, and
SWI-Prolog are required. Model/provider calls are disabled.

## Refresh and verify the mirror

From dotfiles, set `RESEARCH_REPO` to the desired Auto Research checkout:

```sh
python3 .zara/experts/auto-research/build.py --source "$RESEARCH_REPO"
python3 .zara/experts/auto-research/build.py --source "$RESEARCH_REPO" --check
python3 "$RESEARCH_REPO/scripts/check-prolog-mirror.py"
```

The builder mirrors all Git-tracked `roam/**/*.org` files present in the worktree,
including staged new files. Add new Org sources to Git before refreshing.
Untracked files and external symlinks are excluded/rejected. Deleted sources
are removed on refresh. Output is deterministic and partitioned by source tree.
The mirror preserves full text as quoted atoms, source SHA-256 hashes, metadata,
tags, IDs, heading lines, ID links, and normalized relative Org file links.
A manifest records source and output hashes; the repository CI rejects drift.
Only the builder writes `mirror/`. Research text and code blocks are never
executed. Metadata is a source claim, not a verified assertion about the world.

## Ask the expert

```sh
swipl -q -f none -s .zara/experts/auto-research/run.pl -- \
  "$RESEARCH_REPO" design roam/design/star-server/QUASAR-001-basic-typed-graph-editor.org
```

Use `implement` in place of `design` for implementation guidance. The command
checks mirror freshness before loading it. It returns phase-specific actions,
requirement/invariant/boundary/acceptance/failure section references with exact
path/hash/line provenance, transitive linked sources, and blockers for unresolved
links, ambiguous IDs, explicit pending approval, and unsuitable lifecycle states.
Cycles terminate. Plans return at most 16 linked sources and include
`source_limit_reached(16)` if more remain; constraint references cover the selected
document. Query `constraint/4` or plan a linked document for further detail. Missing documents and unsupported phases fail.

For narrower interactive queries, load the repository `.prolog/kb/index.pl`
and this expert's `kb/expert.pl`, then use:

```prolog
auto_research_expert:relevant(auto_research_kb, 'star-server', [quasar], Sources).
auto_research_expert:evidence(auto_research_kb, Path, 'replay', Evidence).
auto_research_expert:constraint(auto_research_kb, Path, Kind, Evidence).
auto_research_expert:plan(auto_research_kb, Path, implement, Plan).
```

Direct queries require the caller to run the freshness check first. `plan/4`
returns `plan(Actions, Blockers, Sources)`. `approval_evidence_required` always
remains: the expert cannot authenticate human approvals or authorize execution.
A lifecycle such as `VERIFIED`/`APPROVED`, a table claim, a merge, and green tests
never substitute for actual review evidence. Schema mentions in mirrored text
are historical claims; resolve the current target consumer lock before coding.
Constraint extraction uses explicit heading cues, not semantic interpretation;
read the referenced sections and current source/tests before implementing.

## Tests

```sh
python3 -m unittest discover -s .zara/experts/auto-research/tests -p 'test_*.py' -v
swipl -q -f none -s .zara/experts/auto-research/tests/expert-tests.pl
```

Both suites are included in the existing expert-library Nix package checks.

## Symbolic conformance

`kb/conformance.pl` interprets finite, ground design-owned `.verify.pl` contracts.
It checks explicit proposal triples for required/forbidden facts, implications,
conflicting single-valued properties such as ownership, and forbidden transitive
flows. Path witnesses explain violations. A candidate must bind the exact design
ID and spec SHA-256. Incomplete projections produce `unknown`; violations produce
`conflict`. The checker never consults or executes spec terms and never changes
a spec to accommodate a proposal.

The agent skill, script, vocabulary, example, and design-hash binding checks are
in Auto Research at `skills/design-conformance/`, `scripts/design-conformance.py`,
and `docs/design-conformance.md`. Those are repository workflow adapters; the
reasoning rules remain here. Natural-language-to-facts interpretation still needs
review, and symbolic consistency does not prove code behavior or observed tests.

Run the additional suite with:

```sh
swipl -q -f none -s .zara/experts/auto-research/tests/conformance-tests.pl
```
