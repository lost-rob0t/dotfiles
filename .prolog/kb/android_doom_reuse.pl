% Termux Doom desktop-reuse architecture (added 2026-11-13, PR dotfiles#352).
% The Termux Doom profile reuses the desktop .doom.d tree read-only; desktop
% Doom is untouched and the profile only launches on Termux.

owner(android_doom_reuse, 'android/doom/config.org').
owner(android_doom_reuse_tests, 'android/tests/test_termux_doom_reuse.py').

% Tangle products live next to the Org source; check-literate-sync enforces
% parity.  config.el structure: termux base -> desktop load -> Android
% overrides, so overrides always win on the phone.
android_doom_structure(termux_base_before_desktop_load).
android_doom_structure(android_overrides_after_desktop_load).

% Android module deviations from desktop init.el: native builds only.
android_doom_deviation(vterm, 'libvterm native build').
android_doom_deviation(pdf, 'poppler/pdf-tools native build').
android_doom_deviation(tree_sitter_flags, 'grammar compilation on device; json/javascript drop +tree-sitter').

% The desktop config aborts unless nsa/lisp-directory points at a real lisp
% tree; config.el pre-binds it from the resolved checkout root because the
% desktop defvar keeps an existing binding.
failure_mode(desktop_config_missing_lisp_dir,
    'desktop config.el adds ~/.dotfiles/lisp to load-path at startup; on checkouts not at ~/.dotfiles the load aborts before any desktop setting applies. Fix: pre-bind nsa/lisp-directory from the checkout before loading desktop config.el').

% Desktop doomdir resolution order in android/doom config.el/packages.el.
deployment_path(android_doom_desktop_dir,
    'STAR_DESKTOP_DOOMDIR env, else ../../.doom.d relative to doom-private-dir, else ~/.dotfiles/.doom.d, else ~/dotfiles/.doom.d').

% Launch surface: termux-doom Chemacs profile (emacs/profiles.org) plus
% bootstrap-created emacs-termux/doom-termux wrappers. Nothing stows this
% profile on the desktop.
launch_constraint(termux_doom_termux_only).
