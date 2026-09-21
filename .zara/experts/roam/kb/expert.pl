:- module(mara_roam_expert, [
    roam_operation/2,
    roam_tool/2,
    roam_plugin/1
]).

text_string(Text, String) :-
    string(Text),
    !,
    String = Text.
text_string(Text, String) :-
    atom(Text),
    atom_string(Text, String).

roam_operation(Text, open_daily) :-
    text_string(Text, String),
    string_lower(String, Lower),
    ( sub_string(Lower, _, _, _, "daily")
    ; sub_string(Lower, _, _, _, "today")
    ; sub_string(Lower, _, _, _, "journal")
    ),
    !.
roam_operation(_, search).

roam_tool(open_daily, 'org_roam.open_daily').
roam_tool(search, 'org_roam.search').

roam_plugin('zara-emacs').
roam_plugin('zara-expert').
