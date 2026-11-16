:- module(dotfiles_expert_tests, []).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(upstream).
:- use_module(dotfiles_expert).

fixture_file('/tmp/zara_dotfiles_fixture/config.nix').

ensure_fixture :-
    fixture_file(F),
    (   exists_file(F)
    ->  true
    ;   file_directory_name(F, D),
        (   exists_directory(D) -> true ; make_directory(D) ),
        open(F, write, S),
        write(S, "{ programs.emacs.enable = true; }\n"),
        close(S)
    ),
    source_code:file_in_repo(F, dotfiles, 'config.nix'),
    !.
ensure_fixture :-
    fixture_file(F),
    assertz(source_code:defines_i(F, programs_emacs_enable, option, h1)),
    assertz(source_code:file_in_repo_i(F, dotfiles, 'config.nix')).

:- begin_tests(dotfiles_expert).

test(upstream_loaded_and_stamped) :-
    upstream_load,
    upstream_ready,
    upstream_info(upstream(_, 'github.com/lost-rob0t/symbolic')).

test(expert_registered_in_moe) :-
    symbolic_rlm:current_registry(R),
    symbolic_rlm:expert_catalog(R, Es),
    member(E, Es),
    symbolic_rlm:contract_id(E, dotfiles_expert).

test(configures_finds_fixture, [setup(ensure_fixture)]) :-
    ensure_fixture,
    zara_expert_dotfiles:dotfiles_handler(dotfiles(configures(programs_emacs_enable), Res), ctx, V),
    V = dotfiles_answer(configures(programs_emacs_enable), Res, success, _Up, _W, _S),
    Res \= [],
    member(file(_F, option), Res).

test(unknown_query_tracked_as_failure, [setup(ensure_fixture)]) :-
    zara_expert_dotfiles:dotfiles_handler(dotfiles(configures(zzz_no_such_option_zz), _), ctx, V),
    V = dotfiles_answer(_, _, failure, _, _, _),
    findall(A, (outcome:attempt(A), outcome:attempt_expert(A, dotfiles_expert)), As),
    length(As, N),
    N >= 2.

test(failure_solutions_surface_on_repeat, [setup(ensure_fixture)]) :-
    catch(zara_expert_dotfiles:dotfiles_handler(dotfiles(configures(yyy_missing_opt), _), _, _), _, true),
    outcome:record_solution('yyy_missing_opt',
                            'mine the dotfiles repository into the symbolic KB',
                            'dotfiles_expert', _S),
    catch(zara_expert_dotfiles:dotfiles_handler(dotfiles(configures(yyy_missing_opt), _), _, V2), _, true),
    V2 = dotfiles_answer(_, _, failure, _, _, Sugg),
    Sugg \= [],
    member(sol(_Sol, Steps, 'dotfiles_expert'), Sugg),
    atom(Steps).

:- end_tests(dotfiles_expert).

:- run_tests(dotfiles_expert),
   halt(0).
