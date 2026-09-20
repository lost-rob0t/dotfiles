# ZARA-CREW/1

Status: implemented editor/Prolog-policy slice on dotfiles PR #280; not a claim
that Consortium #22 or Prolog-RLM #491 already implements the whole runtime.

## Names and ownership

Buffers: `*crew:chat*`, `*crew:files*`, `*crew:kb*`, `*crew:agents-list*`,
`*crew:roles*`. No generic `*chat*`, `*files*` or `*kb*` is reused by the crew entry.
The buffers are projections over one existing Emacs worker registry. Consortium
owns durable orchestration; Prolog-RLM owns generic expert/tool authority.

Role = reusable goal, allowed phases, skills, requested experts, capabilities,
model override and delegable roles. Crew = a snapshot of those roles, project,
model, ARADR phase, capacity, turn allocation and exact source/skill hashes.
Agent instance = host-minted ID with crew/run, parent, depth and remaining turns.
Names do not grant capabilities or authority. Multiple instances may use one role.

## Envelope

All peer requests contain:
`protocol="ZARA-CREW/1"`, `version=1`, `id` (idempotency key), `workspace`,
`crew`, `run`, `epoch`, `from`, `generation`, `method`, `text`.
`message` additionally requires `to`. `spawn` requires `role` and positive integer
`turns`. The host supplies identity fields; model-controlled tool arguments cannot
override them. Protocol/major/operation must match exactly. Text <=8192 UTF-8 bytes;
transport <=64 KiB; ordinary client request <=16 KiB; unknown fields fail at client.

Accepted is NOT completed. Message acknowledgement means mailbox acceptance.
Spawn acknowledgement means policy_pending. Prolog admission may still reject
or become stale; the parent receives the admitted child ID or denial later.
Duplicate request ID plus identical data is a no-op; conflicting reuse fails.
Same-user emacsclient is a trusted-local transport, NOT an OS security sandbox.

## Actors, resources and delegation

Default per crew: 128 total retained members, 8 simultaneous processes, depth 4,
8 children per parent, 512 total allocated turns. Global managed-process ceiling
16; shared registry ceiling 512. Limits are configurable within validated bounds.
One role is not one worker. Kickoff can instantiate a role for many tasks.

Spawn is a typed proposal, not model authority. The Prolog gate checks declared
role, capability subset, depth, fanout, retained member count and available parent
turn allocation. Revalidate the admission snapshot after asynchronous policy
response; debit before child launch. Concurrent proposals cannot duplicate budget.
Root allocations debit the crew pool; children debit the parent allocation.
Cancelled/failed launches do not mint new turns. No automatic allowance reset.
Turn limits are NOT token or dollar limits; provider spend controls remain upstream.

FIFO inboxes reuse the existing actor loop. The ready queue is round-robin and
respects both crew and global concurrency. Parent cancellation cascades through
all descendants and fences pending admissions. History and lineage are retained.
No auto-spawn on Emacs startup. No native OpenCode Task escape hatch. Worker/model
selection goes through the existing `opencode-worker` wrapper with retries off.
That wrapper currently buffers output until exit: do not promise token streaming.

## ARADR

research -> adversarial_review -> analysis -> design -> design_review -> promotion
-> develop -> verify -> completed.

Transitions go through a closed Prolog operation. Reviews require independent
receipts; promotion requires operator-approved evidence; every transition binds
an exact artifact reference and verified receipt supplied by the trusted host.
Peer tools cannot submit approvals. Models cannot self-certify phase transitions.
Finish/cancel phase work before advancing; reset provider session context at each
phase. Existing Consortium ADARD is not silently renamed or reinterpreted.

## Knowledge and skills

The canonical executable crew rules live in `.prolog/kb/crew_protocol_v1.pl`.
The DotfilesExpert `.zara/experts/dotfiles/kb/` adapter re-exports that source;
there is no second copied KB. Full package activation stays with #282.
Every child receives pinned protocol, source hashes, mandatory crew-expert skill,
and only its role-selected custom skills. No source scan is reported as loaded.
Required external experts need a real runtime report through the expert hook;
missing or malformed inventory blocks launch. The full remote tool invocation
and expert replica protocol remains Prolog-RLM #491 / Consortium #22 work.

## Git preservation

All old tracked Prolog paths and bytes remain. Add append-only records or new
versioned modules plus explicit supersession/provenance references. Do not delete,
rename away, truncate, force-rewrite or silently replace past Prolog. Run
`scripts/check-prolog-history.py BASE_SHA` before accepting a branch. CI rejects
deletions/rewrites, symlink replacements and uncommitted or untracked/ignored KB.
No worker may force-push/reset/clean/squash history. Runtime secrets/transcripts
are not made public by this requirement. Git tracking is not permission to publish.

## Implementation limits

Local cancellation kills owned processes and fences results, but cannot assert
that external provider actions or all descendants were rolled back. Receipt and
lineage state in this Emacs slice is process-local; durable cross-host recovery
belongs to Consortium. The separately tracked full expert backend, provider
quotas and live OpenCode tool-discovery smoke remain required integration gates.
