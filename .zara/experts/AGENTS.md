# Expert source ownership

All repo-local expert implementation work belongs under this directory.

- Put authored expert Prolog, deterministic knowledge generators, expert tests,
  and expert packaging beneath `.zara/experts/<expert>/`.
- Keep generic Zara lifecycle/registry/transport code in Zara Core or
  `zara-plugins`; do not move runtime infrastructure here.
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
