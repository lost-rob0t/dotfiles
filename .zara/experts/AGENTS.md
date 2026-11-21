# Expert source ownership

All reusable expert definition/KB implementation work across the user's repositories belongs under this directory.

- Put authored expert Prolog, deterministic knowledge generators, expert tests,
  and expert packaging beneath `.zara/experts/<expert>/`.
- Keep generic Zara lifecycle/registry/transport code in Zara Core or `zara-plugins`, and generic expert runtime/contracts in Prolog-RLM; do not move runtime infrastructure here. Those repositories consume the expert implementations authored here.
- Do not create a second expert manifest/registry contract. Consume the canonical
  `ZARA-EXPERT/1` codec once implemented.
- Reuse durable facts from `.prolog/kb/` through explicit adapters; do not copy
  those facts into expert packages.
- Preserve prior tracked Prolog history. Prefer new versioned modules or adapters
  over destructive rewrites.
- Expert tests must run without a model. Symbolic experts must remain usable with
  model calls disabled.
- Private/session facts, credentials, transcripts, and machine-local state stay
  outside Git.

## Single invocation interface (mandatory)

All expert invocation goes through `bin/zara-expert` (this directory's
`bin/`); do not invent per-pack ad-hoc commands:

- `zara-expert test [PACK]` — merge gate; exit code propagates.
- `zara-expert diagnose K=V...` — symptom facts in, hypothesis/probe/
  verification out.
- `zara-expert coverage --checkout DIR` — journey-spec drift audit;
  non-empty unmapped list fails.
- `zara-expert health --ledger DIR --commits C1,C2` — per-commit suite
  health from a project's run ledger.
- `zara-expert define NAME KIND` — scaffold a new pack when no pack
  fits the domain. Agents MUST define a pack (rather than scattering
  knowledge in prose) whenever a domain produces the same class of
  question three or more times.
