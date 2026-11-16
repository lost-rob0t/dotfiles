:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_home_manager_expert).
test(zara_service_unit_is_owned) :-
    owns_path('/.config/systemd/user/zara-server.service').
test(qwen_conflict_has_bounded_repair_plan) :-
    conflict(qwen3_tts_unit, '/.config/systemd/user/qwen3-tts.service', foreign_regular_file),
    repair_plan(qwen3_tts_unit,
                [backup_foreign_file, remove_foreign_file, home_manager_switch]).
test(configured_backend_is_rocm) :- configured_backend(rocm).
:- end_tests(dotfiles_home_manager_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
