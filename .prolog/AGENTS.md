# Prolog history is durable

Operator requirement: never delete past Prolog; keep it Git tracked.

Keep every existing tracked `.pl` path and byte. Add append-only knowledge or a
fresh versioned module/record with explicit supersession and provenance. Do not
rewrite/truncate/rename away old sources. Do not replace the prior KB with a
fresh model-generated reconstruction. No reset, clean, squash or force-push of
history. New durable KB files, including ignored files, must be committed.

Run `python3 scripts/check-prolog-history.py <actual-base-sha>` before handoff.
Canonical Org sources still own generated Prolog; new incompatible versions get
new Org blocks/output paths, not destructive edits to retained versions.

Separate reviewed facts from raw private transcripts/secrets. Git tracking does
not authorize publication. Crew skills are consumers, not an alternative KB.
