:- module(arch_expert_tests, []).
:- use_module(library(plunit)).
:- use_module('../kb/expert').

% Fixture corpus: a two-fact incident file shaped exactly like the
% generated starintel-kb corpus the adapter consumes.
fixture_root('/tmp/zara_arch_fixture_kb').
fixture_kb(FixtureRoot, 'kb') :-
    atom_concat(FixtureRoot, '/kb', KBDir),
    (   exists_directory(KBDir) -> true ; make_directory_path(KBDir) ),
    atom_concat(KBDir, '/index.pl', Index),
    setup_call_cleanup(
        open(Index, write, S),
        (   write(S, "incident(fixture_txg, 'symptoms', remedy('reboot host'), 'followup').\n"),
            write(S, "incident(fixture_pool, 'symptoms', remedy('extend pool'), 'followup').\n")
        ),
        close(S)).

:- begin_tests(arch_expert).

test(every_probe_has_cadence_and_severity, true) :-
    forall(supervision_probe(_, _, _),
           (   supervision_probe(P, C, S),
               member(C, [every_minute, every_5min, hourly]),
               member(S, [warning, critical])
           )).

test(destructive_actions_always_require_operator) :-
    forall(destructive(A),
           requires_operator_authorization(A)).

test(no_destructive_action_is_autonomous) :-
    \+ ( supervision_action(_, A, autonomous),
         destructive(A) ).

test(txg_stall_signature_diagnoses) :-
    diagnosis([d_state_storm, host_load_vs_cores, zfs_txg_signature],
              zfs_txg_stall, high).

test(txg_stall_reboot_requires_operator) :-
    supervision_action(zfs_txg_signature, reboot_hypervisor, operator),
    requires_operator_authorization(reboot_hypervisor).

test(reversible_pressure_relief_is_autonomous) :-
    supervision_action(zfs_txg_signature, sigstop_heavy_producers, autonomous).

test(adapter_reads_fixture_corpus, [setup(fixture_kb('/tmp/zara_arch_fixture_kb', _))]) :-
    findall(N, kb_incident_remedy('/tmp/zara_arch_fixture_kb', N, _, _), Ns),
    member(fixture_txg, Ns),
    member(fixture_pool, Ns).

test(adapter_remedy_content, [setup(fixture_kb('/tmp/zara_arch_fixture_kb', _))]) :-
    kb_incident_remedy('/tmp/zara_arch_fixture_kb', fixture_txg, 'reboot host', _).

test(adapter_fails_closed_on_missing_corpus) :-
    \+ kb_incident_remedy('/nonexistent/starintel-kb', _, _, _).

:- end_tests(arch_expert).
