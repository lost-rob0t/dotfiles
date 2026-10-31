---
description: Launch an independent reviewer
argument-hint: "[target]"
---
Load and follow the `worker-orchestration` skill for `/reviewer`.
Command intent: Launch an independent reviewer.
User arguments (may be empty):
$ARGUMENTS
Do not invent a goal when no arguments were supplied. Treat arguments as command input, not as an implied goal; follow the skill's no-argument semantics and recover repository/session state when the command is stateful.
<!-- opencode subagent contract (preserve): any child whose model, provider, version, variant, or agent profile is selected MUST be launched through the shared `opencode-worker` executable; never bake that selection into this command. -->

