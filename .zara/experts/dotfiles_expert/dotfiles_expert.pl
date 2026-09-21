:- module(zara_expert_dotfiles, [
              dotfiles_handler/3,
              dotfiles_eval/2
          ]).

:- use_module(library(lists)).
:- use_module(upstream).

dotfiles_handler(dotfiles(Query, Result), _Context, Value) :-
    (   outcome:with_attempt(dotfiles_expert,
                             zara_expert_dotfiles:dotfiles_eval(Query, Result),
                             Report)
    ->  (   Report.outcome == success
        ->  upstream_info(Up),
            Value = dotfiles_answer{query: Query,
                                    result: Result,
                                    outcome: success,
                                    upstream: Up}
        ;   Value = dotfiles_answer{query: Query,
                                    result: none,
                                    outcome: failure,
                                    error: Report.error,
                                    warnings: Report.warnings,
                                    suggested_solutions: Report.solutions}
        )
    ;   Value = dotfiles_answer{query: Query,
                                result: none,
                                outcome: failure,
                                error: attempt_failed,
                                warnings: [],
                                suggested_solutions: []}
    ).

dotfiles_eval(configures(Key), Files) :-
    atom(Key),
    findall(file(File, Kind),
            (   source_code:defines(File, Key, Kind),
                file_in_dotfiles(File)
            ;   documentation:configuration_key(File, Key),
                file_in_dotfiles(File)
            ;   documentation:doc_section(File, Title, _),
                atom(Title),
                downcase_atom(Title, DT),
                downcase_atom(Key, DK),
                sub_atom(DT, _, _, _, DK),
                file_in_dotfiles(File)
            ),
            FilesUnsorted),
    sort(FilesUnsorted, Files),
    not_found(Key, Files).

not_found(_Key, Files) :-
    Files \= [],
    !.
not_found(Key, _Files) :-
    format(atom(Msg), 'no dotfiles configuration found for ~w', [Key]),
    throw(dotfiles_not_found(Msg)).

dotfiles_eval(docs(Topic), Docs) :-
    kb_tools:kb_search_docs(Topic, Docs0),
    include_dotfiles(Docs0, Docs),
    not_found(Topic, Docs).

dotfiles_eval(unit(Unit), Info) :-
    (   computing:service_unit(Unit, Path)
    ->  (   source_code:file_in_repo(Path, dotfiles, _)
        ->  Info = unit(Unit, Path, dotfiles)
        ;   Info = unit(Unit, Path, external)
        )
    ;   format(atom(Msg), 'no systemd unit ~w known to the KB', [Unit]),
        throw(dotfiles_not_found(Msg))
    ).

dotfiles_eval(recent(Within), Changed) :-
    number(Within),
    core:iso_now(Now),
    findall(changed(File, Commit, Time),
            (   source_code:file_in_repo(File, dotfiles, _),
                source_code:last_changed(File, Commit, Time),
                core:time_before(Time, Now),
                core:interval_duration(interval(Time, Now), Age),
                Age =< Within
            ),
            Changed).

dotfiles_eval(context(Topic), Report) :-
    dotfiles_eval(configures(Topic), Files),
    (   dotfiles_eval(docs(Topic), Docs)
    ->  true
    ;   Docs = []
    ),
    Report = context{topic: Topic,
                     files: Files,
                     docs: Docs}.

file_in_dotfiles(File) :-
    source_code:file_in_repo(File, dotfiles, _).

include_dotfiles([], []).
include_dotfiles([doc(D, What)|Rest], [doc(D, What)|Out]) :-
    file_in_dotfiles(D),
    !,
    include_dotfiles(Rest, Out).
include_dotfiles([_|Rest], Out) :-
    include_dotfiles(Rest, Out).

register_dotfiles_expert :-
    upstream_load,
    symbolic_rlm:current_registry(Registry),
    Contract = expert_contract{id: dotfiles_expert,
                               version: '0.1.0',
                               goal: dotfiles/2,
                               priority: 46,
                               requires: [],
                               handler: zara_expert_dotfiles:dotfiles_handler},
    symbolic_rlm:expert_register(Registry, Contract, _),
    catch(symbolic_moe:moe_register(Registry, Contract,
                                    [dotfiles, config, emacs, doom, qtile,
                                     nix, home, manager, stow, unit,
                                     systemd, theme, tangle]),
          _, true).

:- register_dotfiles_expert.
