# ZAGENTS.md

Supplemental plain-text agent gates. The full policy is Prolog (`.prolog/kb/index.pl`) — see `AGENTS.md`. Only rules that must survive a session where Prolog is not loaded live here, and new entries need a real reason, not habit.

1. Org files with `:tangle` are sources of truth; their tangled outputs are generated. Never make a lasting change only in a generated file, and run `scripts/check-literate-sync` before claiming literate work done.
2. Documentation is part of the change: behavior, architecture, ownership, or workflow changes update the relevant docs in the same branch.
3. Run the real tests. Never self-certify success the verifier did not observe.

Anything more specific than the above belongs in the Prolog KB, not here.
