---
description: Immediately stop mutation without destroying state
argument-hint: "[target]"
---
Load and follow the `opencode-orchestrate` skill for `/panic`.
Command intent: Immediately stop mutation without destroying state.
User arguments (may be empty):
$ARGUMENTS
Do not invent a goal when no arguments were supplied. Treat arguments as command input, not as an implied goal; follow the skill's no-argument semantics and recover repository/session state when the command is stateful.
