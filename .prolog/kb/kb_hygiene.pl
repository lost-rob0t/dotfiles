%% Durable KB-authoring hygiene rules for this repository.
%% Promoted 2026-11-15/16 from the divergence-rescue session: consult("index")
%% surfaced syntax errors sitting unnoticed in committed KB files
%% (llm_log.pl, writers.pl); root causes below, both fixed same session.

%% Bare hex git short SHAs are not valid Prolog numbers: 0679212a fails
%% with "operator expected". Single-quote them as atoms.
kb_atom_rule(git_sha_quoted,
    evidence('llm_log.pl lines 7 and 13 broke the loader until the SHAs were quoted'),
    rule('write git short SHAs as single-quoted atoms, never bare hex terms')).

%% An unescaped apostrophe inside a single-quoted atom terminates the atom
%% early; the parser then mis-reads the rest of the file and cascades
%% errors into unrelated clauses.
kb_atom_rule(escape_apostrophes,
    evidence('writers.pl line 3: an unescaped apostrophe in a possessive broke the atom and cascaded syntax errors through the whole file'),
    rule('escape embedded apostrophes as backslash-quote, or rephrase to avoid the possessive')).

%% Loader invariant: the whole KB must consult without errors after every
%% KB edit. Syntax errors hide in files nobody consults until the next
%% agent loads the KB and finds it broken.
kb_load_invariant(consult_index_clean,
    evidence('2026-11-15: errors in llm_log.pl and writers.pl surfaced only when the next consult ran'),
    rule('after any KB edit run: swipl -q -g "consult(index), halt" and require zero ERROR lines')).

%% Known non-blocking loader noise, so agents do not chase it as a defect.
kb_load_invariant(discontiguous_warnings_acceptable,
    evidence('forgejo_ci.pl emits pre-existing discontiguous warnings for workflow_fix/2 clauses (lines 11/20/23)'),
    rule('discontiguous warnings in forgejo_ci.pl are warnings, not errors; consolidation is optional cleanup')).
