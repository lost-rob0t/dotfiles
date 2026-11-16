---
name: distro-council
description: adversarial review, council voting and gated releases for the dotfiles meta-distro; use to vote on milestones or run distro release
compatibility: Any agent harness with filesystem + shell access (OpenCode, Codex, Claude, pi, Zara)
---

# Distro council — adversarial review, voting, gated release

A milestone releases only when the Prolog engine says so.  The council is
the set of independent agents that attack the work and vote.  The engine,
not the council, has final authority: votes and attacks are epoch-stamped
facts, and a gate refuses anything not proven ready.

## Councilor identity

Vote with a stable councilor id (`DISTRO_COUNCILOR` or `--councilor`), e.g.
`opencode/glm-5.3-flash`, `claude/opus`, `codex`.  One councilor, one vote
(last vote wins; change your mind by voting again).

## Attack phase (before any approval)

Attack the code before the PRs — that is the whole point:

1. `git diff` / read the changes behind every done issue in the milestone.
2. Grill them: what is unverified?  What invariant is unstated?  What
   breaks on the flake host, on the phone, on Termux?  Check literate
   parity yourself instead of trusting the author.
3. `distro attack` — runs the repo-configured suite (policy in
   `distro/config.pl`): every step must exit 0 at the current epoch.
4. If you broke something or found a real defect: report it on the issue
   body in `issues.org`, set it back (`STATUS: open` via
   `distro` claim/done cycle by its owner), and vote reject with the
   reason.

## Voting

`distro vote approve "rationale"` / `distro vote reject "reason"`
(optional `--target MILESTONE`).

- Vote only the exact tracked state you reviewed: any change to
  `issues.org` moves the epoch and silently voids all votes.  Re-verify and
  re-vote after changes; the gate enforces this, not politeness.
- Quorum and reject policy live in `distro/config.pl` (`council_quorum/2`
  votes by distinct councilors, zero standing rejects).

## Releasing

When `distro status` shows `release_ready(<M>): yes` **and** you have
personally done the attack phase for that state:

1. `distro release --yes`
2. Commit everything (ledger, `distro/VERSION`, archive, changelog, state
   regenerations are local-only and stay untracked) with a message like
   `distro: release v0.2.0 (ISSUE-0001, ISSUE-0002)`.
3. `--tag` adds a `distro-v<M>` tag; only when the human asked for tags.

## Fail-closed discipline

- Never edit `distro/state/` by hand; it is generated.
- Never weaken `distro/config.pl` (quorum, attack steps) to pass a gate.
  That is the human's deliberate edit, not an agent convenience.
- Never mark evidence green without running the command through
  `distro verify`; the engine stores the digest and epoch, and a release
  audit reads them.
- Old requirements are never hand-deleted: the release archives them to
  `distro/archive/` and they stop being tracked.
