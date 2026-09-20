:- use_module(library(http/json)).
:- use_module('../.prolog/kb/crew_protocol_v1').
:- initialization(main, main).

% One bounded JSON request, closed operations only. Never call/read/consult a
% goal, module or path supplied by a worker. Emacs also bounds process lifetime.
main :-
    catch((json_read_dict(current_input, Request), answer(Request, Answer),
           json_write_dict(current_output, Answer), nl),
          _, (json_write_dict(current_output, _{ok:false, error:"invalid_request"}), nl, halt(1))).

answer(R, _{ok:Allowed}) :-
    R.op == "spawn", !,
    (crew_spawn_allowed(R.parent, R.child, R.limits) -> Allowed=true ; Allowed=false).
answer(R, _{ok:Allowed}) :-
    R.op == "transition", !,
    atom_string(From, R.from), atom_string(To, R.to),
    crew_transition(From, To, R.receipt, Allowed).
answer(R, _{ok:true, protocol:Protocol, phases:Phases}) :-
    R.op == "contract", !,
    crew_protocol(Protocol), findall(Phase, crew_phase(_, Phase), Phases).
