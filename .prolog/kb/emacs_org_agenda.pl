% Durable knowledge: Doom org agenda UI + backlog clearing (org-agenda-ui).
% Promoted 2026-09-19 after org-agenda-revamp verification.

% The *Org Agenda* buffer does NOT run font-lock: org-agenda assigns faces
% with text properties while building the buffer and sets
% font-lock-global-modes to (not org-agenda-mode).  Custom agenda styling
% must use overlays applied from org-finalize-agenda-hook, not
% font-lock-add-keywords or hi-lock.
agenda_style_mechanism(text_properties, overlays_from(org_finalize_agenda_hook)).

% org-agenda overlays that inherit standard faces (font-lock-type-face,
% warning, success, shadow) track theme changes for free; no theme hook is
% needed when the user cycles doom themes.
agenda_ui_theme_adaptivity(inherit_standard_faces).

% Backlog clearing semantics implemented in .doom.d/autoload/org-agenda-ui.el:
backlog_predicate(active_todo_keyword, timestamp_before_today).
backlog_timestamp_sources([scheduled, deadline]).
% org-todo DONE on a repeating entry advances the timestamp one interval per
% call and resets the keyword to the FIRST keyword of the sequence, so a
% clear-backlog command must loop until the entry is no longer overdue and
% restore the original keyword afterwards.
org_repeat_reset_keyword(first_of_sequence).
backlog_loop_guard(max_attempts(366)).

% Doom ERT tests for .doom.d autoload modules run without Doom:
%   emacs -Q --batch -L .doom.d/autoload \
%     -l .doom.d/tests/NAME-test.el -f ert-run-tests-batch-and-exit
% CI pattern: .github/workflows/research-dashboard.yml, org-agenda-ui.yml.
doom_ert_runner(batch, '-L .doom.d/autoload').

% Tangle idempotency check (works while the tree is intentionally dirty):
%   cp OUTPUT /tmp/before && emacs -Q --batch --eval
%   '(progn (require 'org) (require 'ob-tangle)
%      (org-babel-tangle-file "ORG"))' && cmp -s OUTPUT /tmp/before
% `git diff --exit-code <tangled file>` is wrong here when the change is
% uncommitted: it compares against the index, not the tangle.
tangle_parity_check(idempotent_re_tangle_cmp).

% .doom.d/config.org was removed from .literate-sync-baseline after its
% config.el parity was restored (2026-09-19).  .doom.d/packages.org and
% .config/qtile/qtile-ai.org remain baselined drift.
literate_baseline_repaired('.doom.d/config.org', '2026-09-19').
