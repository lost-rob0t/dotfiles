% distro/engine/model.pl -- fact shapes and derived predicates.
%
% State facts are split by authority:
%
%   distro/state/facts.pl   generated from issues.org by "distro sync"
%     issue(ID, Kind, Title, Milestone, Status, Severity)
%     claim(ID, Agent, Time)        -- from the CLAIMED_BY property
%     epoch(Epoch)                  -- sha256 prefix of issues.org at sync
%     distro_version(Version)       -- from distro/VERSION at sync
%
%   distro/state/journal.pl append-only, written by distro subcommands
%     depends_on(ID, Dep)
%     vote(Target, Councilor, Decision, Rationale, Time, Epoch)
%     evidence(ID, Command, ExitCode, Digest, Time, Epoch)
%     attack(Step, ExitCode, Digest, Time, Epoch)
%     drop(ID, Reason, Agent, Time)
%     release(Version, Prev, Time)
%
% Policy facts come from distro/config.pl: council_quorum/1, attack_step/1.

:- dynamic issue/6, claim/3, depends_on/2, vote/6, evidence/6, attack/5,
           drop/4, release/3, epoch/1, distro_version/1.

% ---- issue status views ----
issue_open(ID) :- issue(ID, _, _, _, S, _), memberchk(S, [open, claimed]).
issue_done(ID)  :- issue(ID, _, _, _, done, _).
issue_dropped(ID) :- issue(ID, _, _, _, dropped, _).
issue_dropped(ID) :- drop(ID, _, _, _).

% ---- dependencies: explicit recorded facts, never inferred silently ----
issue_blocked(ID) :-
    depends_on(ID, Dep),
    \+ issue_done(Dep),
    \+ issue_dropped(Dep).

% ---- milestone views ----
milestone_issue(M, ID) :- issue(ID, _, _, M, _, _).
milestone_work(M, ID)  :- milestone_issue(M, ID), \+ issue_dropped(ID).
milestone_open(M, ID)  :- milestone_work(M, ID), \+ issue_done(ID).
milestone_done(M, ID)  :- milestone_work(M, ID), issue_done(ID).
milestone_empty(M)     :- \+ milestone_work(M, _).

% ---- evidence: a done issue needs at least one green recorded observation ----
evidenced(ID) :- evidence(ID, _, 0, _, _, _).

% ---- council: last vote per councilor wins; distinct approvers count ----
latest_vote(Target, C, Decision) :-
    vote(Target, C, Decision, _, T, _),
    \+ (vote(Target, C, _, _, T2, _), T2 @> T).
approve_vote(Target, C) :- latest_vote(Target, C, approve).
approvals(Target, Cs) :- setof(C, approve_vote(Target, C), Cs), !.
approvals(_, []).
approval_count(Target, N) :- approvals(Target, Cs), length(Cs, N).
standing_reject(Target) :- latest_vote(Target, _, reject).

% Votes only count for the exact tracked state they reviewed.
votes_current(Target) :-
    epoch(E),
    forall(latest_vote(Target, _, _), vote(Target, _, _, _, _, E)).

% ---- work selection: highest priority, unblocked, unclaimed first ----
severity_rank(high, 0). severity_rank(medium, 1). severity_rank(low, 2).
next_candidate(ID) :- issue_open(ID), \+ issue_blocked(ID), \+ claim(ID, _, _).
next_issue(ID) :-
    findall(R-ID,
            (next_candidate(ID),
             issue(ID, _, _, _, _, Sev), severity_rank(Sev, R)),
            L),
    msort(L, S), S = [_-ID|_].
