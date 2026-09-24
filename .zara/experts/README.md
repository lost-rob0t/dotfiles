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
- `music/` — zero-model media intent routing over the canonical Zara media tools,
  with fresh now-playing context required before music discussion/recommendation.
- `proxmox/` — typed Proxmox observation/mutation/destructive policy, explicit
  approval/postcondition rules, and environment/wallet/auth-source-only credentials.
- `prolog/` — module/predicate/DCG diagnostics with parser/xref/compiler evidence,
  explicit no-`call/1` source execution, generation fences, and fresh re-xref
  repair verification.
- `python/` — AST/scope/import/type-hint semantics with optional configured-only
  lint/type adapters, generation-fenced observations, and read-only generated
  sources.
- `nim/` — proc/func/template/macro/type/import reasoning with typed compiler
  probes, generation-fenced evidence, and fresh `nim check` repair verification.
- `lisp/` — shared reader-aware structural state machine for delimiter diagnosis
  and typed missing-close repair previews. It ignores delimiters in strings,
  escaped strings, line comments, nested `#| |#` comments, Common Lisp `#\\`
  character literals, and Emacs Lisp `?\\` character literals. It never writes.
- `common-lisp/` — Common Lisp package/reader/defining-form specialization over
  `LispExpert`; structural repair delegates to `zara:expert/lisp` and requires
  fresh SBCL reader/compile evidence before success can be claimed.
- `emacs-lisp/` — Emacs Lisp forms, lexical-binding/autoload/style specialization
  over `LispExpert`; structural repair delegates to `zara:expert/lisp` and
  requires fresh batch-Emacs reader/byte-compile evidence before success.

The Prolog/Python/Nim expert brains are the canonical Dotfiles-owned sources for
`prolog-rlm#495/#498/#499`; `zara-plugins#872` consumes them through the existing
registered-predicate host boundary instead of owning duplicate language KBs.
All three pin providers disabled and `max_model_calls=0` / `model_calls=0`.

The Lisp family is the canonical Dotfiles-owned source for
`prolog-rlm#494/#496/#497`; `zara-plugins#869` owns only the Zara adapter and
registered-predicate capability boundary. `CommonLispExpert` and
`EmacsLispExpert` do not fork structural parser semantics: their repair preview
path delegates to `LispExpert` under the same zero-model budget, while dialect
success remains contingent on fresh real reader evidence.

Their adapter-ready operation ABI is also canonical here: `language_applicable/3`,
`language_evidence/3`, `language_diagnostic/3`, `language_repair_preview/4`,
`language_repair_verify/4`, `language_style_rules/3`, and
`language_explanation/3`. Shared deterministic ABI helpers live under
`language/kb/adapter_contract.pl`; each expert exports language-specific wrappers.
Inspection is observation-only, repair preview never writes, and repair verification
returns `verified(false)` until fresh host-owned parser/compiler/xref postcondition
evidence exists. `zara-plugins` may adapt these predicates, but must not fork their
brain semantics, registry, permission path, provider runtime, or usage ledger.

The Lisp-family adapter surface consumed by `zara-plugins#869` is deliberately
narrow and stable: `can_handle/2`, `structural_check/2`, `structural_diagnose/2`,
`preview_repair/3`, `verify_repair/3`, `style_rules/2`, and `explain_decision/2`.
All three brains pin providers disabled and `max_model_calls=0` / `model_calls=0`.
No predicate applies edits; Zara Core retains expected-preimage capability,
approval, generation fencing, and fresh postcondition ownership.

MusicExpert and ProxmoxExpert are Dotfiles-owned domain brains for the
Zara feature lab. Product-facing MPRIS and Proxmox adapters remain downstream
plugin work; these brains do not create a second runtime, registry, or effect path.

The expert-library Nix package runs every tracked Prolog expert test and also
executes parser-only fixtures for the Bash and Nix packages (`bash -n` and
`nix-instantiate --parse`). It additionally runs the Lisp-family SWI suites, reads
valid/broken Common Lisp fixtures with real SBCL, reads valid/broken Emacs Lisp
fixtures with batch Emacs, and byte-compiles the valid Emacs Lisp fixture with
warnings treated as errors. Those checks validate syntax without evaluating the
inspected Lisp forms. Evaluation, write, and other effectful operations remain
host-owned capabilities and are not implied by an expert source package.

The Emacs corpus implementation was moved here from experimental
`zara-plugins#858` so the expert knowledge source belongs to dotfiles. The
service-plugin repository should consume/package this expert instead of owning a
second copy.

DotfilesExpert remains tracked by `dotfiles#281/#282`; it should reuse these
packages and `.prolog/kb/` rather than fork their knowledge.