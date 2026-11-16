% kb-ingest / opencode-session-to-org writer lessons (fixed 37850118, 2026-11-02)
writer_bug(emacsclient_env,
    'emacsclient --eval does not forward the client environment: any (getenv "X") evaluated server-side returns nil. Writers must generate a form file that setenv\'s every KB_ROAM_* variable (backslash/quote-escaped via the lisp_str helper) and send only (load-file "<escaped path>"). Batch fallback uses --load on the same file.').
writer_bug(prolog_underscores_in_shell,
    'Uppercase identifiers after "-" in writeln probes (e.g. writeln(vp-FAILED)) print as unbound variables (_NNN) - Prolog parses FAILED as a variable. Use lowercase atoms in diagnostic terms.').
writer_bug(dict_function_eq,
    '=( Dict.get(k), Var ) does not evaluate the dict function; Var binds to the raw compound. Use get_dict(k, Dict, Var). ==/2 does evaluate dict functions, so tests can pass while the client code is broken.').
writer_bug(unquoted_flag_literals,
    'A clause pattern element written --flag parses as the compound -(flag) and never matches split_string tokens; quote flag literals as "--flag".').
emacs_daemon_fact('2026-11-02: two wedged Emacs daemons hung all emacsclient evals (kb-ingest queued indefinitely). Recovery: pkill -9 -x emacs, restart emacs --init-directory ~/.config/emacs --daemon=qtile, rm stale ~/.emacs.d/server/server, ln -sf qtile ~/.emacs.d/server/server so default clients reach the named daemon. home-manager switch is broken (forgejo issue #146) so the rebuilt kb-ingest PATH binary lands only when HM recovers; until then the fixed dotfiles scripts are invoked directly.').
