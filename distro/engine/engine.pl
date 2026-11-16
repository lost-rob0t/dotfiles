% distro/engine/engine.pl -- loader for the dotfiles meta-distro release engine.
%
% Two supported entrances:
%
% 1. CLI (scripts/distro): `swipl -s distro/engine/engine.pl -g <goal>` with
%    cwd anywhere; DISTRO_ROOT overrides the checkout root (sandboxes).
%
% 2. Any live Prolog session (prolog MCP / prolog-rlm inside OpenCode):
%    ?- [distro/engine/engine].
%    ?- distro_root(R), release_ready('0.2.0').
%    ?- reload_distro_state.        % after "distro sync" changed state files
%    ?- set_distro_root('/path/to/other/checkout'), load_distro_state.
%
% The root is derived from this file's own location unless DISTRO_ROOT is
% set, so sessions can consult the engine without caring about cwd.

:- dynamic user:distro_root/1.

:- prolog_load_context(directory, Dir),
   (getenv('DISTRO_ROOT', EnvRoot) ->
        Root = EnvRoot
    ;   atomics_to_string([Dir, '/../..'], Rel),
        absolute_file_name(Rel, Root, [file_type(directory)])),
   assertz(user:distro_root(Root)),
   working_directory(_, Dir),
   maplist(consult, ['../config', 'model', 'version', 'gates', 'report']).

set_distro_root(Root) :-
    retractall(user:distro_root(_)),
    assertz(user:distro_root(Root)),
    load_distro_state.

state_file('facts.pl').
state_file('journal.pl').

state_path(File, Path) :-
    user:distro_root(Root),
    atomics_to_string([Root, '/distro/state/', File], Path).

safe_consult(Path) :-
    (exists_file(Path) ->
         consult(Path)
     ;   format(user_error,
                'distro engine: missing ~w (run "distro sync" first)~n', [Path])).

load_distro_state :-
    forall(state_file(F),
           (state_path(F, P), safe_consult(P))).

% Re-read generated state after "distro sync" mutated it (consult only
% retracts clauses attributed to the consulted file).
reload_distro_state :- load_distro_state.

% Auto-load state once the helpers above exist.
:- load_distro_state.
