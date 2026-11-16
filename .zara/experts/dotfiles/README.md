# DotfilesExpert

Canonical project-domain symbolic brain for `zara:expert/dotfiles`.

This package owns deterministic dotfiles classification, durable-KB provenance,
project ownership reasoning, specialist delegation hints, and project style source
references. It does **not** own Zara's expert registry, lifecycle, scheduler,
permission system, provider runtime, conversation history, or effect execution.
Those remain in Zara Core / `zara-plugins`.

## Reused sources

The brain imports reviewed durable facts from `.prolog/kb/` rather than copying
them. `kb_source/5` records the exact project-relative source and revision used by
the package. Unknown layouts or paths fail closed; the downstream adapter returns
a typed unknown/unsupported result rather than calling a model.

## Delegation

`specialist_for/4` returns canonical `zara:expert/*` identities only. Nix and Bash
are currently covered because their exact canonical brains and adapter ABI are
already landed. Invocation, shared budgets, cycle detection, cancellation, and
evidence chaining are downstream runtime responsibilities.

## Style

Project style lives under the canonical `.zara/style/` root:

- `project.pl` — project-wide narrowing rules;
- `languages/nix.pl` — Nix-specific style/provenance;
- `languages/bash.pl` — Bash-specific style/provenance.

These files are inert symbolic data modules. They grant no authority and do not
execute effects. Their revisions are exposed through `style_source/4` so the
runtime can preserve source/scope/revision provenance when building overlays.

## Pure-symbolic invariant

`provider_policy(disabled)`, `max_model_calls(0)`, and `model_calls(0)` are hard
facts. This package requires no provider credentials and contains no model or
network fallback.
