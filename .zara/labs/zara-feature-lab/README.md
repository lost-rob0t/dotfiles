# Zara Feature Lab

This is the Dotfiles-owned incubation lane for Zara plugin work. It launches
**exactly five** isolated workers from `features.json`:

1. Prolog-RLM / symbolic runtime integration
2. MPRIS + music conversation
3. Bash tool + BashExpert integration
4. Proxmox tool + ProxmoxExpert integration
5. Nix tool + NixExpert integration

## Runtime invariant

Every worker is admitted only after `scripts/zara-feature-lab-runtime.pl`
loads the public **Prolog-RLM** entrypoint and the worker's canonical Dotfiles
expert. Admission fails unless the runtime reports ready and the expert proves:

- `provider_policy(disabled)`
- `max_model_calls(0)`
- `model_calls(0)`

That is the current pure-symbolic gate. Do not fabricate a second
direct/symbolic/symbolic-recursive selector while the canonical Prolog-RLM mode
contract is still upstream work.

The coding worker itself may use its configured model after symbolic admission;
the expert brain does not. Prolog-RLM is the authoritative symbolic/control-plane
runtime; OpenCode is only the isolated mutation executor until AgentProlog's
headless coding workflow is available. Each worker runs via
`opencode-worker --mode isolated-mutate` in its own linked Git worktree.

## Prototype layout

A worker develops its promotable delta under:

`.zara/labs/zara-feature-lab/features/<id>/overlay/`

The overlay mirrors files relative to that feature's `promotion_path` in the
`zara-plugins` repository. Canonical expert brain changes remain under
`.zara/experts/` in Dotfiles and are consumed downstream; they are not copied
into plugin adapters.

## Promotion

`zara-feature-lab promote <id>` verifies the Dotfiles worker worktree, creates
an isolated `zara-plugins` worktree/branch, copies only that feature's overlay,
commits the candidate, then records registry/focused-test/diff evidence through
`prolog-verify observe` and requires `prolog-verify check` at that exact commit.
It does **not** push, merge, force-push, or write master.

Credentials and tokens never belong in this tree. Proxmox and other runtime
secrets must come from environment variables, wallet/keyring, or Emacs
auth-source.
