
% Doom default entry (verified 2026-10-03, gen ee61a9db era).
% ~/.local/bin/emacs (hm-deployed doomEmacsWrapper) execs emacs with
% --init-directory ~/.config/emacs and DOOMDIR=~/.doom.d; ~/.doom.d and ~/lisp
% are symlinks into ~/.dotfiles, so config.el + lisp/llm load from the repo.
entrypoint(doom_default, '~/.local/bin/emacs --init-directory ~/.config/emacs, DOOMDIR=~/.doom.d').
failure_mode(vanilla_daemon_squats_doom_socket,
    'a daemon started as bare `emacs --daemon=doom` (no --init-directory) serves socket doom as vanilla emacs; symptom: doom "broken" while fresh interactive boots work. Fix: kill squatter, restart via ~/.local/bin/emacs --daemon=doom').
infra_quirk(doom_core_has_no_init_el,
    'doom core checkout at ~/.config/emacs legitimately has no init.el (upstream dropped it; early-init.el is the bootstrapper); `emacs -l init.el` batch tests are invalid').
