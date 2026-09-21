:- begin_tests(mara_roam_expert).

:- use_module('../kb/expert.pl').

test(today_routes_to_daily) :-
    roam_operation("open today's daily note", open_daily),
    roam_tool(open_daily, 'org_roam.open_daily').

test_search_is_default) :-
    roam_operation("find symbolic memory notes", search),
    roam_tool(search, 'org_roam.search').

test(uses_emacs_and_symbolic_host) :-
    roam_plugin('zara-emacs'),
    roam_plugin('zara-expert').

:- end_tests(mara_roam_expert).

:- run_tests,
   halt.
