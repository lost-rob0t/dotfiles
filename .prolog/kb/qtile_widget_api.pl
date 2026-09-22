% Durable knowledge: Qtile widget API on this machine (qtile 0.36.1.dev).
% Verified live 2026-09-19 while building qtile_audio.py.

% libqtile.widget.base has NO ThreadedPollText in 0.36; the threaded poll
% base is BackgroundPoll (poll() runs in an executor).
qtile_poll_base('0.36', base.BackgroundPoll).

% base._Widget.button_press dispatches mouse_callbacks["ButtonN"] as plain
% callables or LazyCalls.  Scroll is Button4/Button5.
qtile_widget_mouse(mouse_callbacks, ['Button1', 'Button2', 'Button4', 'Button5']).

% libqtile.popup.Popup is subclassable; override process_button_click(x, y,
% button) for row-index dropdowns.  vertical_padding etc. only exist after
% super().__init__, so compute popup height from the raw config dict first.
qtile_popup_dropdown(override(process_button_click)).
qtile_popup_init_order(compute_height_before_super_init).

% Setq'ing byte-compile-dest-file-function via let inside --eval fails
% (exit 255).  Use (progn (setq byte-compile-dest-file-function ...)
% (byte-compile-file F)) to keep .elc output out of the worktree, which
% keeps the prolog-verify worktree digest stable.
emacs_byte_compile_out_of_tree(setq(byte_compile_dest_file_function)).

% The system qtile interpreter is /usr/bin/python (has libqtile); plain
% python3 does not.  Widget tests use AST structure checks plus behavior
% checks run through /usr/bin/python subprocesses.
qtile_test_interpreter('/usr/bin/python', has_libqtile(true)).

% Audio control on this machine: pactl addresses PipeWire; @DEFAULT_SINK@
% targets the active output, `pactl list sinks` parses in "Sink #" blocks,
% and the pinned default is stored under $XDG_STATE_HOME/nsa-qtile/.
audio_backend(pactl, target('@DEFAULT_SINK@')).
audio_state_file('~/.local/state/nsa-qtile/audio-default-sink').

% qtile-audio.org -> qtile_audio.py is the canonical literate pair for the
% bar audio widget; config.py imports Audio inside init_widgets_list.
qtile_audio_ownership('qtile-audio.org', 'qtile_audio.py').
