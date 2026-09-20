:- module(dotfiles_home_manager_expert,
    [ owns_path/1, conflict/3, repair_plan/2, configured_backend/1 ]).

:- ensure_loaded('../../../../.prolog/kb/home_manager_ownership.pl').

owns_path(Path) :- hm_owns_path(Path).
conflict(Unit, Path, foreign_regular_file) :- hm_link_conflict(Unit, Path).
repair_plan(Unit, [backup_foreign_file, remove_foreign_file, home_manager_switch]) :-
    hm_link_conflict(Unit, _).
configured_backend(Backend) :- hm_configured_backend(Backend).
