#!/usr/bin/env swipl
/** zara-expert: the single invocation interface for expert systems.

Every agent session uses this one entry point; no per-expert bespoke
invocation. Subcommands:

    zara-expert test [PACK]              Run a pack's model-free tests (default: quasar-ui).
    zara-expert diagnose [PACK] K=VALUE...  Observe facts (default pack: quasar-ui;
                                         runs PACK/probes.sh when no facts given),
                                         print hypothesis/probe/verification.
    zara-expert coverage --checkout DIR  Audit: journey specs on disk vs the surface registry.
    zara-expert health --ledger DIR --commits C1,C2,...   Per-commit suite health from the run ledger.
    zara-expert define NAME KIND         Scaffold a new expert pack under .zara/experts/NAME/.
    zara-expert confirm --ledger DIR HYP  Record that a hypothesis was confirmed by acting evidence.
    zara-expert refute  --ledger DIR HYP NOTE  Record a refutation with why; chain flags suspect.
    zara-expert chains  --ledger DIR     Per-chain confirmed/refuted record + suspect flags.

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
dispatch(diagnose, Args0) :- !,
    script_base(Base),
    (   Args0 = [Pack0 | Rest],
        atom_string(PackStr, Pack0),
        \+ sub_atom(PackStr, _, 1, _, '=')
    ->  Pack = PackStr
    ;   Pack = 'quasar-ui', Rest = Args0 ),
    format(atom(Expert), '~a/~a/kb/expert', [Base, Pack]),
    use_module(Expert),
    (   Rest == []
    ->  (   probe_command(Base, Pack, Cmd)
        ->  probe_facts(Cmd, Facts)
        ;   Facts = Rest )
    ;   Facts = Rest ),
    forall(member(Pair, Facts),
           ( split_pair(Pair, K, V),
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
dispatch(confirm, Args) :- !,
    option_value(ledger, Args, LedgerDir),
    positional_args(Args, Positionals),
    ( Positionals = [Hyp] -> true ; usage, halt(2) ),
    ( option_value(note, Args, Note0) -> true ; Note0 = "" ),
    atom_string(Note, Note0),
    script_base(Base),
    format(atom(LedgerMod), '~a/quasar-ui/kb/run_ledger', [Base]),
    use_module(LedgerMod),
    record_consultation(LedgerDir, Hyp, confirmed, Note),
    chain_record(Hyp, Confirmed, Refuted),
    format("~w confirmed (record: ~w confirmed / ~w refuted)~n", [Hyp, Confirmed, Refuted]),
    halt(0).
dispatch(refute, Args) :- !,
    option_value(ledger, Args, LedgerDir),
    positional_args(Args, Positionals),
    ( Positionals = [Hyp] -> true ; usage, halt(2) ),
    ( option_value(note, Args, Note0), Note0 \= "" -> true
    ; writeln("refute requires --note \"why it was wrong (evidence)\""), halt(2) ),
    atom_string(Note, Note0),
    script_base(Base),
    format(atom(LedgerMod), '~a/quasar-ui/kb/run_ledger', [Base]),
    use_module(LedgerMod),
    record_consultation(LedgerDir, Hyp, refuted, Note),
    format("~w REFUTED - chain flagged suspect for future diagnose calls~n", [Hyp]),
    halt(0).
dispatch(chains, Args) :- !,
    option_value(ledger, Args, LedgerDir),
    script_base(Base),
    format(atom(LedgerMod), '~a/quasar-ui/kb/run_ledger', [Base]),
    use_module(LedgerMod),
    load_run_ledger(LedgerDir),
    ( chain_hypotheses(Hyps) ->
        forall(( member(H, Hyps),
                 chain_record(H, Confirmed, Refuted) ),
               ( ( suspect_chain(H) -> Sus = 'SUSPECT (latest: refuted)' ; Sus = '' ),
                 format("~w: ~w confirmed, ~w refuted ~w~n", [H, Confirmed, Refuted, Sus]) )),
        halt(0)
    ; writeln("no consultations recorded yet (confirm/refute build the record)"),
      halt(0)
    ).
dispatch(define, [Name, Kind]) :- !,
    scaffold(Name, Kind),
    format("scaffolded .zara/experts/~a/ - author kb/expert.pl rules and tests/expert-tests.pl pins, then add the pack to .zara/experts/README.md~n", [Name]).
dispatch(_, _) :- usage, halt(2).

usage :-
    writeln("usage: zara-expert test [PACK] | diagnose [PACK] [K=V...] | coverage --checkout DIR"),
    writeln("             health --ledger DIR --commits C1,C2 | define NAME KIND"),
    writeln("             confirm/refute --ledger DIR HYP [--note NOTE] | chains --ledger DIR").

%% positional_args(+Args, -Positionals): drops '--flag value' pairs
%% (swipl pre-splits '--opt=value'), leaving bare positionals.
positional_args([], []).
positional_args([Flag, _Value | Rest], Positionals) :-
    sub_atom(Flag, 0, 2, _, '--'), !,
    positional_args(Rest, Positionals).
positional_args([Other | Rest], [Other | Positionals]) :-
    positional_args(Rest, Positionals).

%% option_value(Name, Args, Value): swipl pre-splits --name=VALUE into
%% [--name, VALUE], so accept both the split and the joined forms.
option_value(Name, Args, Value) :-
    format(atom(Flag), '--~a', [Name]),
    append(_, [Flag, Value | _], Args), !.
option_value(Name, Args, Value) :-
    format(atom(Prefix), '--~a=', [Name]),
    member(Arg, Args),
    atom_concat(Prefix, Value, Arg), !.

%% split_pair(Pair, K, V): split a K=V argv item; values may contain '='.
split_pair(Pair, K, V) :-
    split_string(Pair, "=", "", [K0 | VParts]),
    atom_string(K, K0),
    atomic_list_concat(VParts, "=", V1),
    term_string(V, V1).

%% probe_command(Base, Pack, Cmd): shell command that emits the pack's
%% K=V observation facts, when the pack ships probes.sh.
probe_command(Base, Pack, Cmd) :-
    format(atom(Probes), '~a/~a/probes.sh', [Base, Pack]),
    exists_file(Probes),
    format(atom(Cmd), "sh '~a'", [Probes]).

%% probe_facts(Cmd, Facts): run Cmd, collect its non-empty output lines.
probe_facts(Cmd, Facts) :-
    setup_call_cleanup(open(pipe(Cmd), read, In),
                       read_probes(In, Facts),
                       close(In)).

read_probes(In, Facts) :-
    read_line_to_string(In, Line),
    ( Line == end_of_file -> Facts = []
    ; string_trim(Line) == "" -> read_probes(In, Facts)
    ; Facts = [Line | Tail], read_probes(In, Tail) ).

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
