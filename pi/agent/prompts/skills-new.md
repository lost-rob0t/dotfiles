---
description: Create a reusable local or global skill
argument-hint: "[target]"
---
Load and follow the `skill-scope` skill for `/skills-new`.
Command intent: Create a reusable local or global skill.
User arguments (may be empty):
$ARGUMENTS
Do not invent a goal when no arguments are supplied. Use project-local .agents/skills (repo) unless global scope is explicit; the global editable source is ~/skills (git checkout). Never edit the symlinked deployment roots (~/.agents/skills, ~/.config/opencode/skills).
