:- use_module('../.prolog/kb/crew_protocol_v1').
:- begin_tests(crew_protocol).
parent(_{delegates:["reviewer"],capabilities:["read","spawn"],remaining:8,depth:0,children:0}).
child(_{role:"reviewer",capabilities:["read"],turns:4}).
limits(_{max_depth:4,max_children:8,max_agents:128,members:32}).
test(protocol) :- crew_protocol('ZARA-CREW/1').
test(admits) :- parent(P),child(C),limits(L),crew_spawn_allowed(P,C,L).
test(escalation,[fail]) :- parent(P),child(C),limits(L),crew_spawn_allowed(P,C.put(capabilities,["write"]),L).
test(budget,[fail]) :- parent(P),child(C),limits(L),crew_spawn_allowed(P,C.put(turns,9),L).
test(depth,[fail]) :- parent(P),child(C),limits(L),crew_spawn_allowed(P.put(depth,4),C,L).
test(children,[fail]) :- parent(P),child(C),limits(L),crew_spawn_allowed(P.put(children,8),C,L).
test(registry,[fail]) :- parent(P),child(C),limits(L),crew_spawn_allowed(P,C,L.put(members,128)).
test(role,[fail]) :- parent(P),child(C),limits(L),crew_spawn_allowed(P,C.put(role,"admin"),L).
test(promotion_requires_operator) :- crew_transition(promotion,develop,_{verified:true,artifact:"sha256:x",operator_approved:false},false).
test(review_requires_independence) :- crew_transition(design_review,promotion,_{verified:true,artifact:"sha256:x",independent:false},false).
test(no_skip) :- crew_transition(research,develop,_{verified:true,artifact:"sha256:x"},false).
test(approved) :- crew_transition(promotion,develop,_{verified:true,artifact:"sha256:x",operator_approved:true},true).
:- end_tests(crew_protocol).
:- initialization((run_tests -> halt(0) ; halt(1)), main).
