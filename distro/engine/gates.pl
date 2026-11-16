% distro/engine/gates.pl -- release gates.  Fail closed: everything not
% proven ready blocks the release, and every blocker is reportable.

% Attack suite: every configured step needs a green observation at the
% current epoch.
attack_current(Step) :- attack_step(Step), epoch(E), attack(Step, 0, _, _, E).
attack_stale(Step) :- attack_step(Step), \+ attack_current(Step).
attack_passed :- \+ attack_stale(_).

% One clause per reason; release_ready(M) fails closed on any of them.
release_blocker(M, empty_milestone) :-
    milestone_empty(M).
release_blocker(M, open_issue(ID)) :-
    milestone_open(M, ID).
release_blocker(M, unevidenced(ID)) :-
    milestone_done(M, ID), \+ evidenced(ID).
release_blocker(M, unmet_dependency(ID, Dep)) :-
    milestone_work(M, ID), depends_on(ID, Dep),
    \+ issue_done(Dep), \+ issue_dropped(Dep).
release_blocker(M, stale_votes) :-
    once(milestone_work(M, _)), \+ votes_current(M).
release_blocker(M, standing_reject(C)) :-
    once(milestone_work(M, _)), latest_vote(M, C, reject).
release_blocker(M, quorum(Have, Need)) :-
    once(milestone_work(M, _)), council_quorum(Need),
    approval_count(M, Have), Have < Need.
release_blocker(M, attack_stale(Step)) :-
    once(milestone_work(M, _)), attack_stale(Step).

release_ready(M) :- \+ release_blocker(M, _).

% Target resolution: the next minor of the current version when tracked
% work targets it; otherwise the lowest milestone holding work.
release_target(M) :-
    distro_version(V), bump_version(V, minor, M),
    \+ milestone_empty(M), !.
release_target(M) :-
    setof(Cand, ID^milestone_work(Cand, ID), [M|_]), !.
