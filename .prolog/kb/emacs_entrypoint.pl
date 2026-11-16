
% Doom default entry (verified 2026-10-03, gen ee61a9db era).
% ~/.local/bin/emacs (hm-deployed doomEmacsWrapper) execs emacs with
% --init-directory ~/.config/emacs and DOOMDIR=~/.doom.d; ~/.doom.d and ~/lisp
% are symlinks into ~/.dotfiles, so config.el + lisp/llm load from the repo.
entrypoint(doom_default, '~/.local/bin/emacs --init-directory ~/.config/emacs, DOOMDIR=~/.doom.d').
failure_mode(vanilla_daemon_squats_doom_socket,
    'a daemon started as bare `emacs --daemon=doom` (no --init-directory) serves socket doom as vanilla emacs; symptom: doom "broken" while fresh interactive boots work. Fix: kill squatter, restart via ~/.local/bin/emacs --daemon=doom').

% Single-server consolidation (2026-11-12): qtile autostart, emacs_ui popups,
% and the scratchpad eclient/org-capture helpers share ONE daemon on the
% DEFAULT server socket ("server"); QTILE_EMACS_SERVER still overrides popup
% clients. Merged to master 4e7301fc (branch fix/emacs-single-server-doom-warnings).
invariant(single_default_emacs_daemon,
    'exactly one doom daemon started via ~/.local/bin/emacs --daemon; never --daemon=qtile, never a bare/raw emacs --daemon (vanilla ~/.emacs.d squatter wedges client handshakes on a listening socket)').
failure_mode(wedged_daemon_hangs_emacsclient,
    'a listening server socket that never completes the handshake hangs every emacsclient (no -a fallback fires because the socket exists). Probe with `timeout 5 emacsclient -a false --eval t`: rc 124 => wedged => kill daemon, restart via ~/.local/bin/emacs --daemon').

% melpa upstream renamed master -> main; straight clones are single-branch on
% the old name, so plain `git fetch origin` fails and `doom sync` aborts.
fix(straight_melpa_main_branch,
    'git -C ~/.config/emacs/.local/straight/repos/melpa config remote.origin.fetch "+refs/heads/main:refs/remotes/origin/main" && git fetch origin && git symbolic-ref refs/remotes/origin/HEAD refs/remotes/origin/main; then doom sync succeeds (lockfile pin stays reachable from main history)').

% Live hazard observed 2026-11-12: concurrent opencode/claude agent sessions
% on ~/.dotfiles repeatedly revert .doom.d/config.org to the pre-fix committed
% blob (cp-style restore preserving an old mtime). Commits were safe; worktree
% edits were not.
hazard(agent_sessions_revert_literate_sources,
    'check git status/diff for .doom.d/config.org before AND after editing it in ~/.dotfiles; re-verify fixes still present before tangling or trusting worktree state; restore with git checkout -- .doom.d/config.org').
infra_quirk(doom_core_has_no_init_el,
    'doom core checkout at ~/.config/emacs legitimately has no init.el (upstream dropped it; early-init.el is the bootstrapper); `emacs -l init.el` batch tests are invalid').
