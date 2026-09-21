:- module(mara_router, [
    route_text/2,
    family_plugin/2,
    family_tool/2,
    coding_default_plugins/1
]).

text_string(Text, String) :-
    string(Text),
    !,
    String = Text.
text_string(Text, String) :-
    atom(Text),
    atom_string(Text, String).

tokens(Text, Tokens) :-
    text_string(Text, String),
    string_lower(String, Lower),
    split_string(Lower, " \t\n\r.,:;!?/()[]{}_-", " \t\n\r.,:;!?/()[]{}_-", Tokens).

contains_any(Tokens, Words) :-
    member(Token, Tokens),
    memberchk(Token, Words),
    !.

route_text(Text, todo) :-
    tokens(Text, Tokens),
    contains_any(Tokens, ["todo", "task", "tasks", "remind", "reminder", "due", "deadline", "schedule"]),
    !.
route_text(Text, roam) :-
    tokens(Text, Tokens),
    contains_any(Tokens, ["roam", "note", "notes", "notebook", "memory", "kb", "knowledgebase"]),
    !.
route_text(Text, transit) :-
    tokens(Text, Tokens),
    contains_any(Tokens, ["bus", "cota", "transit", "stop", "stops", "departure", "departures", "route"]),
    !.
route_text(Text, search) :-
    tokens(Text, Tokens),
    contains_any(Tokens, ["search", "web", "brave", "lookup", "research"]),
    !.
route_text(Text, coding) :-
    tokens(Text, Tokens),
    contains_any(Tokens, [
        "code", "coding", "repo", "repository", "git", "branch", "test", "tests",
        "build", "debug", "fix", "lisp", "prolog", "nix", "python", "javascript",
        "typescript", "java", "kotlin", "bash", "shell"
    ]),
    !.
route_text(Text, emacs) :-
    tokens(Text, Tokens),
    contains_any(Tokens, ["emacs", "buffer", "window", "keybinding", "command"]),
    !.
route_text(_, general).

family_plugin(todo, 'zara-emacs').
family_plugin(todo, 'zara-timers').
family_plugin(todo, 'zara-expert').

family_plugin(roam, 'zara-emacs').
family_plugin(roam, 'zara-expert').

family_plugin(transit, 'zara-cota').
family_plugin(transit, 'zara-mobility').

family_plugin(search, 'zara-knowledge').

family_plugin(emacs, 'zara-emacs').

family_plugin(coding, 'zara-coding').
family_plugin(coding, 'zara-shell').
family_plugin(coding, 'zara-github').
family_plugin(coding, 'zara-files').
family_plugin(coding, 'zara-context').
family_plugin(coding, 'zara-emacs').
family_plugin(coding, 'zara-expert').
family_plugin(coding, 'zara-bash-expert').
family_plugin(coding, 'zara-java-expert').
family_plugin(coding, 'zara-javascript-expert').
family_plugin(coding, 'zara-kotlin-expert').
family_plugin(coding, 'zara-nix-expert').
family_plugin(coding, 'zara-typescript-expert').

family_plugin(general, 'zara-context').
family_plugin(general, 'zara-memory').

family_tool(todo, 'org_todo.list').
family_tool(todo, 'org_todo.capture').
family_tool(todo, 'org_todo.state').
family_tool(todo, 'org_todo.snapshot').
family_tool(todo, 'timers.reminder').
family_tool(roam, 'org_roam.search').
family_tool(roam, 'org_roam.open_daily').
family_tool(transit, 'cota.routes').
family_tool(transit, 'cota.route').
family_tool(transit, 'cota.route_stops').
family_tool(transit, 'cota.departures').
family_tool(transit, 'mobility.route').
family_tool(search, 'knowledge.search').

coding_default_plugins(Plugins) :-
    findall(Plugin, family_plugin(coding, Plugin), Plugins).
