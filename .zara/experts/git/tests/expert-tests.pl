:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_git_expert).
test(status_is_read_only) :- read_only(status).
test(fetch_uses_network_and_updates_refs) :-
    network_operation(fetch),
    git_effect(fetch, none, none, write, true, false).
test(rebase_rewrites_history_and_needs_clean_tree) :-
    rewrites_history(rebase),
    requires_clean_worktree(rebase).
test(force_push_requires_explicit_authorization) :-
    destructive(force_push),
    precondition(force_push, explicit_operator_authorization).
test(commit_is_not_read_only, [fail]) :- read_only(commit).
:- end_tests(dotfiles_git_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
