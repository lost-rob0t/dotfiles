:- module(dotfiles_git_expert,
    [ git_effect/6,
      read_only/1,
      network_operation/1,
      rewrites_history/1,
      destructive/1,
      requires_clean_worktree/1,
      precondition/2
    ]).

% git_effect(Operation, Worktree, Index, Refs, Network, HistoryRewrite).
git_effect(status, none, none, none, false, false).
git_effect(diff, none, none, none, false, false).
git_effect(log, none, none, none, false, false).
git_effect(show, none, none, none, false, false).
git_effect(fetch, none, none, write, true, false).
git_effect(add, none, write, none, false, false).
git_effect(commit, none, read, write, false, false).
git_effect(switch, write, none, read, false, false).
git_effect(merge, write, write, write, false, false).
git_effect(pull, write, write, write, true, false).
git_effect(rebase, write, write, write, false, true).
git_effect(reset_hard, write, write, write, false, true).
git_effect(clean, delete, none, none, false, false).
git_effect(push, none, none, remote_write, true, false).
git_effect(force_push, none, none, remote_write, true, true).

read_only(Operation) :-
    git_effect(Operation, none, none, none, false, false).

network_operation(Operation) :-
    git_effect(Operation, _, _, _, true, _).

rewrites_history(Operation) :-
    git_effect(Operation, _, _, _, _, true).

destructive(reset_hard).
destructive(clean).
destructive(force_push).

requires_clean_worktree(switch).
requires_clean_worktree(merge).
requires_clean_worktree(pull).
requires_clean_worktree(rebase).

precondition(Operation, inspect_status) :-
    \+ read_only(Operation).
precondition(Operation, clean_or_stash_worktree) :-
    requires_clean_worktree(Operation).
precondition(force_push, explicit_operator_authorization).
precondition(reset_hard, explicit_operator_authorization).
precondition(clean, explicit_operator_authorization).
