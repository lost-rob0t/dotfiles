# Repo-local Zara experts

This directory is the canonical source tree for project-owned Zara expert work:
authored Prolog knowledge/rules, deterministic corpus builders, expert-specific
tests, and packaging.

It is intentionally **not** a second Zara expert registry. `ZARA-EXPERT/1`
descriptor/activation semantics remain owned by `lost-rob0t/zara#1233` (current
Core implementation PR `lost-rob0t/zara#1273`), and canonical root/discovery
semantics by `lost-rob0t/zara#1249`.

## Ownership

- `.zara/experts/` owns repo-local expert implementations.
- `.prolog/kb/` remains durable project memory. Experts may import reviewed KB
  modules from there; do not copy those facts into a second truth store.
- Zara Core and `zara-plugins` own runtime, discovery, lifecycle, and adapter code.
  New personal/project expert corpora and rules belong here.
- Generated corpora are build artifacts unless an explicit provenance policy says
  otherwise. Builders and tests are tracked; private runtime facts are not.
- Existing tracked Prolog history must not be deleted or rewritten merely to
  reorganize expert packages.

## Current experts

- `emacs/` — reproducible Emacs self-documentation corpus plus bounded queries.
- `git/` — symbolic Git operation/effect/precondition reasoning.
- `home-manager/` — adapter over the existing durable Home Manager ownership KB.
- `zara/` — adapter over existing Zara deployment and voice-validation KB facts.
- `sysadmin/` — symptom → hypothesis → diagnostic → verification rules.
- `javascript/` / `typescript/` — canonical JS/TS language semantics.
- `java/` / `kotlin/` — canonical JVM language semantics.
- `bash/` — shell grammar/startup/quoting semantics with parse-only inspection.
- `nix/` — Nix/flake/module/Home Manager reasoning with build/eval kept explicit.
- `prolog/` — module/predicate/DCG diagnostics with parser/xref/compiler evidence,
  explicit no-`call/1` source execution, generation fences, and fresh re-xref
  repair verification.
- `python/` — AST/scope/import/type-hint semantics with optional configured-only
  lint/type adapters, generation-fenced observations, and read-only generated
  sources.
- `nim/` — proc/func/template/macro/type/import reasoning with typed compiler
  probes, generation-fenced evidence, and fresh `nim check` repair verification.

The Prolog/Python/Nim expert brains are the canonical Dotfiles-owned sources for
`prolog-rlm#495/#498/#499`; `zara-plugins#872` consumes them through the existing
registered-predicate host boundary instead of owning duplicate language KBs.
All three pin providers disabled and `max_model_calls=0` / `model_calls=0`.

The expert-library Nix package runs every tracked Prolog expert test and also
executes parser-only fixtures for the Bash and Nix packages (`bash -n` and
`nix-instantiate --parse`). Those checks validate syntax without sourcing shell
code or evaluating/building Nix expressions. Evaluation, build, write, and other
effectful operations remain host-owned capabilities and are not implied by an
expert source package.

The Emacs corpus implementation was moved here from experimental
`zara-plugins#858` so the expert knowledge source belongs to dotfiles. The
service-plugin repository should consume/package this expert instead of owning a
second copy.

DotfilesExpert remains tracked by `dotfiles#281/#282`; it should reuse these
packages and `.prolog/kb/` rather than fork their knowledge.
