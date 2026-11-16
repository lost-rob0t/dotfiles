---
description: Improve an existing local or global skill
argument-hint: "[target]"
---
Load and follow the `skill-edit` and `skill-scope` skills for `/skill-edit`.
Command intent: Improve an existing local or global skill.
User arguments (may be empty):
$ARGUMENTS
Do not invent a goal when no arguments are supplied. Resolve local and global origins explicitly; edit global skills only in ~/skills (the git checkout), never the symlinked deployment roots. Changes go on a branch in ~/skills; validate with the repo validate-skills gate.
