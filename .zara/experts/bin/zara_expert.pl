#!/usr/bin/env swipl
/** zara-expert: the single invocation interface for expert systems.

Every agent session uses this one entry point; no per-expert bespoke
invocation. Subcommands:

    zara-expert test [PACK]              Run a pack's model-free tests (default: quasar-ui).
    zara-expert diagnose KEY=VALUE...    Observe symptom facts, print hypothesis/probe/verification.
    zara-expert coverage --checkout DIR  Audit: journey specs on disk vs the surface registry.
    zara-expert health --ledger DIR --commits C1,C2,...   Per-commit suite health from the run ledger.
    zara-expert define NAME KIND         Scaffold a new expert pack under .zara/experts/NAME/.

Conventions the interface enforces:
- Expert packs live in .zara/experts/<name>/{kb,tests}[/corpus|/bin].
- Tests are model-free and network-free; `test` is the merge gate and
  its exit code propagates.
- New knowledge lands as facts/rules in the right pack; when no pack
  fits, `define` scaffolds one rather than letting knowledge scatter.
*/
:- use_module(library(lists)).

%% Base dir (.zara/experts) resolved from this script's own location, so
%% every subcommand works regardless of the caller's cwd.
:- dynamic script_base/1.
:- prolog_load_context(file, File),
   file_directory_name(File, BinDir),
   file_directory_name(BinDir, Base),
   assertz(script_base(Base)).

main :-
    current_prolog_flag(argv, Argv),
    strip_swipl_flags(Argv, Args),
    ( Args = [Command | Rest] -> dispatch(Command, Rest)
    ; usage, halt(2)
    ).

strip_swipl_flags([X|T], Rest) :- atom(X), sub_atom(X, 0, 1, _, '-'), !, strip_swipl_flags(T, Rest).
strip_swipl_flags(T, T).

dispatch(test, Args) :- !,
    ( Args = [Pack] -> true ; Pack = 'quasar-ui' ),
    script_base(Base),
    format(atom(Goal), "cd '~a/~a' && swipl -q -t halt -g \"run_tests\" tests/expert-tests.pl", [Base, Pack]),
    shell(Goal, Status),
    ( Status = 0 -> halt(0) ; halt(Status) ).
dispatch(diagnose, Facts) :- !,
    script_base(Base),
    format(atom(Expert), '~a/quasar-ui/kb/expert', [Base]),
    use_module(Expert),
    forall(member(Pair, Facts),
           ( split_string(Pair, "=", "", [K0, V0]),
             atom_string(K, K0),
             term_string(V, V0),
             observe(K, V) )),
    ( findall(H, hypothesis(H), []) ->
        writeln("no hypothesis fires for the observed facts; add facts or author a new chain"),
        halt(1)
    ; forall(hypothesis(H),
             ( format("hypothesis: ~w~n", [H]),
               ( next_probe(H, P) -> format("  next probe: ~w~n", [P]) ; true ),
               ( verification(H, V) -> format("  verification: ~w~n", [V]) ; true ) ))
    ),
    reset_facts.
dispatch(coverage, Args) :- !,
    option_value(checkout, Args, Checkout),
    script_base(Base),
    format(atom(Expert), '~a/quasar-ui/kb/expert', [Base]),
    format(atom(Corpus), '~a/quasar-ui/corpus/build_corpus', [Base]),
    use_module(Expert),
    use_module(Corpus),
    findall(S, surface_spec(_, S), Mapped0), sort(Mapped0, Mapped),
    unmapped_specs(Checkout, Mapped, Unmapped),
    ( Unmapped == [] -> writeln("coverage: every tracked journey spec is mapped")
    ; writeln("UNMAPPED journey specs (register them in kb/expert.pl):"),
      forall(member(S, Unmapped), format("  ~w~n", [S])),
      halt(1)
    ).
dispatch(health, Args) :- !,
    option_value(ledger, Args, LedgerDir),
    option_value(commits, Args, Commits0),
    split_string(Commits0, ",", "", CommitStrings),
    maplist(atom_string, OrderedCommits, CommitStrings),
    script_base(Base),
    format(atom(LedgerMod), '~a/quasar-ui/kb/run_ledger', [Base]),
    use_module(LedgerMod),
    load_run_ledger(LedgerDir),
    forall(member(C, OrderedCommits),
           ( ( commit_health(C, H) -> format("~w: ~w~n", [C, H])
             ; format("~w: no recorded runs~n", [C]) )
           )),
    halt(0).
dispatch(define, [Name, Kind]) :- !,
    scaffold(Name, Kind),
    format("scaffolded .zara/experts/~a/ - author kb/expert.pl rules and tests/expert-tests.pl pins, then add the pack to .zara/experts/README.md~n", [Name]).
dispatch(_, _) :- usage, halt(2).

usage :-
    writeln("usage: zara-expert test [PACK] | diagnose K=V... | coverage --checkout DIR"),
    writeln("             health --ledger DIR --commits C1,C2 | define NAME KIND").

%% option_value(Name, Args, Value): swipl pre-splits --name=VALUE into
%% [--name, VALUE], so accept both the split and the joined forms.
option_value(Name, Args, Value) :-
    format(atom(Flag), '--~a', [Name]),
    append(_, [Flag, Value | _], Args), !.
option_value(Name, Args, Value) :-
    format(atom(Prefix), '--~a=', [Name]),
    member(Arg, Args),
    atom_concat(Prefix, Value, Arg), !.

%% scaffold(Name, Kind): minimal pack skeleton; the agent fills the KB.
scaffold(Name, Kind) :-
    Dir = '.zara/experts',
    format(atom(PackDir), '~a/~a', [Dir, Name]),
    format(atom(Kb), '~a/kb', [PackDir]),
    format(atom(Tests), '~a/tests', [PackDir]),
    make_directory(PackDir), make_directory(Kb), make_directory(Tests),
    format(atom(Module), 'dotfiles_~a_expert', [Name]),
    format(atom(Expert), '~a/kb/expert.pl', [PackDir]),
    format(atom(TestsFile), '~a/tests/expert-tests.pl', [PackDir]),
    open(Expert, write, S1),
    format(S1, "%% ~a expert: authored rules over ~a concerns.~n", [Name, Kind]),
    format(S1, "%% Model-free; tests are the gate. See .zara/experts/AGENTS.md.~n"),
    format(S1, ":- module(~a, [ fact/2, observe/2, reset_facts/0 ]).~n~n", [Module]),
    format(S1, ":- dynamic fact/2.~n~n"),
    format(S1, "observe(Key, Value) :-~n    retractall(fact(Key, _)),~n    assertz(fact(Key, Value)).~n~n"),
    format(S1, "reset_facts :- retractall(fact(_, _)).~n"),
    close(S1),
    open(TestsFile, write, S2),
    format(S2, ":- use_module(library(plunit)).~n:- use_module('../kb/expert').~n~n"),
    format(S2, ":- begin_tests(~a, [setup(reset_facts), cleanup(reset_facts)]).~n~n", [Module]),
    format(S2, "test(observe_and_reset) :-~n    observe(example_key, example_value),~n    fact(example_key, example_value),~n    reset_facts,~n    \\+ fact(example_key, _).~n~n"),
    format(S2, ":- end_tests(~a).~n~n"),
    format(S2, ":- initialization(main, main).~nmain :- (run_tests -> halt(0) ; halt(1)).~n"),
    close(S2).

:- initialization(main, main).
