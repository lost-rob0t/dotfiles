% Termux native SWI-Prolog (arm64-android, 10.1.19) build quirks.
% Verified 2026-11-05 while building pi/agent/extensions/rlm-ops drivers.

% predicate_property/2 with Name/Arity indicators resolves to unrelated
% yall imports and reports dynamic predicates as static/imported.
infra_quirk(predicate_property_broken,
    'predicate_property(Name/Arity, dynamic) is unreliable on arm64-android swipl; track dynamic predicates in an explicit registry instead of querying properties').

% call_with_time_limit wrapping a goal inside findall yields only the first
% solution (backtracking is cut by the alarm cleanup).
infra_quirk(call_with_time_limit_breaks_findall,
    'findall(X, call_with_time_limit(T, Goal), _) returns one solution on arm64-android swipl; enforce timeouts by killing the process, not in Prolog').

% Positional .pl arguments after a script are CONSULTED as extra files:
% directives in agent-written .pl arguments would execute.
infra_quirk(positional_pl_args_consulted,
    'swipl script.pl other.pl consults other.pl; pass driver arguments as JSON through environment variables (KB_ARGS/GATE_ARGS pattern), never positional .pl argv').

% Verified-working primitives on the same build.
verified_primitive(assertz_undefined_auto_dynamic, 'assertz on an undefined predicate creates it dynamic').
verified_primitive(flock_serializes, 'flock -x -w N lock swipl ... serializes concurrent one-shot drivers').
verified_primitive(argv_atoms, 'current_prolog_flag(argv,..) yields atoms; convert before comparing to strings').
verified_primitive(read_term_stream_loader, 'manual read_term/assertz loading with an explicit predicate registry round-trips shared fact files').
