:- begin_tests(mara_router).

:- use_module('../kb/router.pl').

test(todo_routes_symbolically) :-
    route_text("what tasks are due today", todo).

test(roam_beats_generic_search) :-
    route_text("search my notes for actor model", roam).

test(cota_routes_to_transit) :-
    route_text("when is the next COTA bus departure", transit),
    family_plugin(transit, 'zara-cota').

test(brave_routes_to_search) :-
    route_text("brave search Prolog tabling", search),
    family_plugin(search, 'zara-knowledge').

test(coding_gets_full_default_tool_family) :-
    route_text("debug the failing git tests", coding),
    coding_default_plugins(Plugins),
    maplist(
        {Plugins}/[Plugin]>>memberchk(Plugin, Plugins),
        ['zara-coding', 'zara-shell', 'zara-github', 'zara-files',
         'zara-context', 'zara-emacs', 'zara-expert']
    ).

test(todo_exposes_org_and_reminder_tools) :-
    family_tool(todo, 'org_todo.list'),
    family_tool(todo, 'timers.reminder').

:- end_tests(mara_router).

:- run_tests,
   halt.
