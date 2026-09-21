:- module(zara_expert_dotfiles_upstream, [
              upstream_load/0,
              upstream_ready/0,
              upstream_info/1
          ]).

:- use_module(library(lists)).

:- dynamic upstream_loaded/1.

symbolic_home(Home) :-
    (   getenv('SYMBOLIC_HOME', Env)
    ->  Home = Env
    ;   expand_file_name('~/Documents/AI/symbolic', [Home0]),
        Home = Home0
    ).

upstream_load :-
    upstream_loaded(Home),
    !,
    upstream_ready_check(Home).
upstream_load :-
    symbolic_home(Home),
    format(atom(KB), '~w/prolog/symbolic.pl', [Home]),
    (   exists_file(KB)
    ->  true
    ;   throw(error(upstream_missing(KB), context(upstream_load, _)))
    ),
    (   current_predicate(user:kb_status/1)
    ->  true
    ;   consult(KB)
    ),
    asserta(upstream_loaded(Home)).

upstream_ready_check(Home) :-
    (   current_predicate(user:kb_status/1)
    ->  true
    ;   format(atom(KB), '~w/prolog/symbolic.pl', [Home]),
        consult(KB)
    ).

upstream_ready :-
    upstream_loaded(_).

upstream_info(upstream(Home, 'github.com/lost-rob0t/symbolic')) :-
    upstream_loaded(Home).
