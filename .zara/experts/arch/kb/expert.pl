:- module(dotfiles_arch_expert,
    [ supervision_probe/3,        % Probe, Cadence, Severity
      diagnosis/3,                % Findings, Diagnosis, Confidence
      supervision_action/3,       % Finding, Action, Authorization
      destructive/1,              % Action
      requires_operator_authorization/1,
      kb_incident_remedy/4        % adapter over a StarIntel KB corpus
    ]).

/** <module> Infrastructure architecture and supervision expert

Decision tables for supervising the StarIntel infrastructure: which probes
to run, how to read findings into a diagnosis, and which recovery actions
are safe to take autonomously versus which always require the operator.
Incident history is consumed through an explicit adapter over a
consolidated starintel-kb corpus (never copied here).

Bootstrapped 2026-11-04 from the lostprox ZFS txg-stall outage and the
starintel-server #38 drain-resilience deployment.
*/

% ---------------------------------------------------------------------------
% Probes: what supervision watches. Severity orders alerting.
% ---------------------------------------------------------------------------

supervision_probe(host_load_vs_cores,   every_minute, critical).
supervision_probe(d_state_storm,        every_minute, critical).
supervision_probe(zfs_txg_signature,    every_minute, critical).
supervision_probe(pool_capacity,        every_5min,   warning).
supervision_probe(smart_health,         hourly,       warning).
supervision_probe(ct_liveness,          every_minute, critical).
supervision_probe(service_health,       every_minute, critical).
supervision_probe(ingest_worker_stops,  every_5min,   warning).
supervision_probe(quarantine_growth,    every_5min,   warning).

% ---------------------------------------------------------------------------
% Findings -> diagnosis. Findings are a sorted list of ground atoms.
% ---------------------------------------------------------------------------

diagnosis([d_state_storm, host_load_vs_cores, zfs_txg_signature],
          zfs_txg_stall, high).
diagnosis([d_state_storm, host_load_vs_cores],
          host_io_wedge_unexplained, medium).
diagnosis([host_load_vs_cores],
          host_overloaded, low).
diagnosis([ingest_worker_stops, quarantine_growth],
          ingest_settlement_failure, medium).
diagnosis([quarantine_growth],
          producer_poison_or_duplicate_storm, low).
diagnosis([service_health],
          service_down, medium).

% ---------------------------------------------------------------------------
% Actions. Authorization is either autonomous (read-only, reversible) or
% operator (anything disruptive or host-wide). Supervision NEVER escalates
% to operator-authorized actions on its own.
% ---------------------------------------------------------------------------

% Finding, Action, Authorization
supervision_action(zfs_txg_signature,      collect_wchan_stack_report, autonomous).
supervision_action(zfs_txg_signature,      verify_pool_and_smart_clean, autonomous).
supervision_action(zfs_txg_signature,      sigstop_heavy_producers,     autonomous).  % reversible
supervision_action(zfs_txg_signature,      reboot_hypervisor,           operator).    % whole-host blast radius
supervision_action(zfs_txg_signature,      hard_reset_hypervisor,       operator).

supervision_action(host_overloaded,        identify_top_consumers,      autonomous).
supervision_action(service_down,           probe_health_endpoints,      autonomous).
supervision_action(service_down,           restart_service_container,   operator).

supervision_action(ingest_settlement_failure, sample_quarantine_records, autonomous).
supervision_action(ingest_settlement_failure, check_worker_supervision,  autonomous).
supervision_action(ingest_settlement_failure, restart_server_container,  operator).

supervision_action(producer_poison_or_duplicate_storm, sample_quarantine_records, autonomous).
supervision_action(producer_poison_or_duplicate_storm, throttle_producer, autonomous). % reversible pause

% Destructive or host-wide actions always require the operator, even when
% they are the known remedy for an incident.
destructive(reboot_hypervisor).
destructive(hard_reset_hypervisor).
destructive(restart_service_container).
destructive(restart_server_container).

requires_operator_authorization(Action) :-
    supervision_action(_, Action, operator).
requires_operator_authorization(Action) :-
    destructive(Action).

% ---------------------------------------------------------------------------
% Incident-history adapter over a consolidated starintel-kb corpus.
%
% The corpus is generated output (starintel-kb tools/consolidate.py); this
% adapter consults it read-only and projects incident/4 facts. It never
% copies corpus facts into this expert. Corpus root resolves from the
% STARINTEL_KB_ROOT environment variable, defaulting to the canonical
% workspace checkout.
% ---------------------------------------------------------------------------

default_kb_root('/home/unseen/starintel/starintel-kb').

kb_root(Root) :-
    (   getenv('STARINTEL_KB_ROOT', Root)
    ->  true
    ;   default_kb_root(Root)
    ).

kb_incident_index(Root, Index) :-
    atom_concat(Root, '/kb/index.pl', Index).

%% kb_incident_remedy(+CorpusRoot, -Name, -Remedy, -Followup) is nondet.
%
% Project every incident/4 from the corpus. Fails closed (no facts) when
% the corpus is absent rather than guessing.
kb_incident_remedy(CorpusRoot, Name, Remedy, Followup) :-
    kb_incident_index(CorpusRoot, Index),
    exists_file(Index),
    setup_call_cleanup(
        ensure_loaded(Index),
        incident(Name, _Symptoms, remedy(Remedy), Followup),
        true).
