% distro/engine/report.pl -- human-readable reporting for status/release.

report_issue_line(ID) :-
    issue(ID, Kind, Title, M, Status, Sev),
    format('  ~w [~w/~w] ~w (milestone ~w) -- ~w~n', [ID, Kind, Sev, Status, M, Title]).

report_milestone(M) :-
    findall(ID, milestone_open(M, ID), Open),
    findall(ID, milestone_done(M, ID), Done),
    length(Open, NO), length(Done, ND),
    format('milestone ~w: ~d open, ~d done~n', [M, NO, ND]),
    forall(member(ID, Open), report_issue_line(ID)),
    forall(member(ID, Done), report_issue_line(ID)).

report_status :-
    (distro_version(V) ->
         format('version: ~w~n', [V])
     ;   format('version: unsynced~n')),
    (epoch(E) ->
         format('epoch: ~w~n', [E])
     ;   format('epoch: unsynced (run "distro sync")~n')),
    (setof(M, ID^milestone_issue(M, ID), Ms) ->
         forall(member(M, Ms), report_milestone(M))
     ;   format('no tracked issues~n')).

report_release :-
    (release_target(M) ->
         format('release target: ~w~n', [M]),
         (release_ready(M) ->
              format('release_ready(~w): yes~n', [M])
          ;   format('release_ready(~w): no~n', [M]),
              forall(release_blocker(M, B), format('  blocker: ~w~n', [B])))
     ;   format('release target: none (no tracked work)~n')),
    (next_issue(ID) ->
         format('next:~n'), report_issue_line(ID)
     ;   format('next: none~n')).
