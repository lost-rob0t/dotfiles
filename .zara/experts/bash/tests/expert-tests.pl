:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_bash_expert).

test(identity_and_upstream) :-
    expert_id('zara:expert/bash'),
    upstream_contract('lost-rob0t/prolog-rlm#502').

test(shell_extensions_are_explicit) :-
    accepts_extension(sh),
    accepts_extension(bash).

test(zsh_is_not_silently_accepted, [fail]) :-
    accepts_extension(zsh).

test(startup_files_keep_distinct_semantics) :-
    startup_file('.bash_profile', login_shell),
    startup_file('.profile', login_shell_fallback),
    startup_file('.bashrc', interactive_nonlogin).

test(symbolic_diagnostics_cover_shell_semantics) :-
    supports_semantic(quoting),
    supports_semantic(word_splitting),
    supports_semantic(globbing),
    supports_semantic(arrays),
    supports_semantic(redirection),
    supports_semantic(pipelines),
    supports_semantic(startup_source_graph).

test(read_only_inspection_never_executes_source) :-
    inspection_policy(parse_only),
    source_execution_policy(never).

test(parser_probe_is_read_only) :-
    parser_probe(bash_n, read_only).

test(repairs_require_fresh_parse_postcondition) :-
    repair_verification(parse_and_bash_n).

test(pure_symbolic_budget_is_closed) :-
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

:- end_tests(dotfiles_bash_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
