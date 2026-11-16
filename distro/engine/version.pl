% distro/engine/version.pl -- semver parsing and bumping over atoms like '0.1.0'.

semver_parts(V, A-B-C) :-
    split_string(V, ".", "", [SA, SB, SC]), !,
    atom_number(SA, A), atom_number(SB, B), atom_number(SC, C).

bump_version(V, major, New) :-
    semver_parts(V, A-_-_), A1 is A + 1,
    atomic_list_concat([A1, 0, 0], '.', New).
bump_version(V, minor, New) :-
    semver_parts(V, A-B-_), B1 is B + 1,
    atomic_list_concat([A, B1, 0], '.', New).
bump_version(V, patch, New) :-
    semver_parts(V, A-B-C), C1 is C + 1,
    atomic_list_concat([A, B, C1], '.', New).
