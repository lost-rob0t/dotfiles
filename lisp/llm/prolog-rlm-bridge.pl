:- module(prolog_rlm_bridge, [main/0]).

:- use_module(library(http/json)).
:- use_module(library(lists)).

:- initialization(main, main).

main :-
    catch(run, Exception, write_bridge_exception(Exception)).

run :-
    json_read_dict(current_input, Request),
    require_text(Request, operation, Operation),
    dispatch(Operation, Request, Response),
    write_json(Response).

dispatch("status", Request, Response) :-
    !,
    load_rlm(Request),
    rlm:rlm_version(Version),
    (   rlm:rlm_ready
    ->  Ready = true
    ;   Ready = false
    ),
    text_value(Version, VersionText),
    Response = _{ok:true,
                 operation:"status",
                 ready:Ready,
                 version:VersionText}.
dispatch("completion", Request, Response) :-
    !,
    load_rlm(Request),
    require_text(Request, query, Query),
    require_text(Request, context, Context),
    request_model(Request, Model),
    request_budget(Request, Budget),
    rlm_chain:openrouter_provider(Model, Provider),
    root_capabilities(Capabilities),
    child_capabilities(ChildCapabilities),
    planner_instruction(PlannerInstruction),
    Options = [ provider(Provider),
                provider_name(openrouter),
                capabilities(Capabilities),
                child_capabilities(ChildCapabilities),
                planner_instruction(PlannerInstruction),
                planner_attempts(2),
                planner_max_tokens(1536),
                context_options([max_results(64),
                                 max_bytes(32768),
                                 time_limit(1.0)]),
                budget(Budget)
              ],
    rlm:rlm_completion(Query, text(Context), Options, Outcome),
    outcome_response(Outcome, Response).
dispatch(Operation, _, _) :-
    throw(error(domain_error(prolog_rlm_bridge_operation, Operation), _)).

load_rlm(Request) :-
    require_text(Request, root, Root0),
    (   absolute_file_name(Root0,
                           Root,
                           [ file_type(directory),
                             access(read),
                             file_errors(fail)
                           ])
    ->  true
    ;   throw(error(existence_error(directory, Root0), _))
    ),
    directory_file_path(Root, 'prolog/rlm.pl', RlmFile),
    (   exists_file(RlmFile)
    ->  true
    ;   throw(error(existence_error(source_sink, RlmFile), _))
    ),
    load_files(RlmFile, [silent(true)]).

request_model(Request, Model) :-
    (   get_dict(model, Request, Model0),
        text_value(Model0, ModelText),
        ModelText \== ""
    ->  atom_string(Model, ModelText)
    ;   Model = 'openrouter/free'
    ).

request_budget(Request, Budget) :-
    (   get_dict(budget, Request, Requested),
        is_dict(Requested)
    ->  true
    ;   Requested = _{}
    ),
    bounded_integer(Requested, max_iterations, 32, 1, 64, MaxIterations),
    bounded_integer(Requested, max_recursion_depth, 1, 0, 1, MaxDepth),
    bounded_integer(Requested, max_concurrent_subcalls, 2, 1, 4, MaxConcurrent),
    bounded_integer(Requested, max_model_calls, 4, 1, 8, MaxModelCalls),
    bounded_integer(Requested, max_tool_calls, 0, 0, 0, MaxToolCalls),
    bounded_integer(Requested, max_context_ops, 8, 1, 32, MaxContextOps),
    bounded_integer(Requested, max_total_tokens, 8192, 256, 32768, MaxTokens),
    bounded_number(Requested, max_cost_usd, 0.25, 0.0, 1.0, MaxCost),
    bounded_integer(Requested, max_output_bytes, 65536, 1024, 262144, MaxOutput),
    bounded_number(Requested, time_limit, 60.0, 1.0, 120.0, TimeLimit),
    Budget = completion_budget{
                 max_iterations:MaxIterations,
                 max_recursion_depth:MaxDepth,
                 max_concurrent_subcalls:MaxConcurrent,
                 max_model_calls:MaxModelCalls,
                 max_tool_calls:MaxToolCalls,
                 max_context_ops:MaxContextOps,
                 max_total_tokens:MaxTokens,
                 max_cost_usd:MaxCost,
                 max_output_bytes:MaxOutput,
                 time_limit:TimeLimit
             }.

bounded_integer(Dict, Key, Default, Min, Max, Value) :-
    (   get_dict(Key, Dict, Candidate),
        integer(Candidate)
    ->  clamp(Candidate, Min, Max, Value)
    ;   Value = Default
    ).

bounded_number(Dict, Key, Default, Min, Max, Value) :-
    (   get_dict(Key, Dict, Candidate),
        number(Candidate)
    ->  clamp(Candidate, Min, Max, Value)
    ;   Value = Default
    ).

clamp(Value, Min, _, Min) :- Value < Min, !.
clamp(Value, _, Max, Max) :- Value > Max, !.
clamp(Value, _, _, Value).

root_capabilities([ rlm,
                    model(openrouter),
                    context(peek),
                    context(slice),
                    context(search),
                    context(partition),
                    context(map),
                    context(reduce)
                  ]).

child_capabilities([model(openrouter)]).

planner_instruction(
"Use the opaque context whenever the answer depends on supplied context. Inspect it with bounded context operations rather than guessing from metadata. Prefer deterministic context search/slicing before model recursion. Recurse only when decomposition materially helps. Return a final answer grounded in inspected context.").

outcome_response(ok(Result), Response) :-
    !,
    json_safe(Result, Safe),
    Response = _{ok:true,
                 kind:"rlm_result",
                 result:Safe}.
outcome_response(error(Error), Response) :-
    !,
    json_safe(Error, Safe),
    Response = _{ok:false,
                 kind:"rlm_error",
                 error:Safe}.
outcome_response(Other, Response) :-
    json_safe(Other, Safe),
    Response = _{ok:false,
                 kind:"invalid_rlm_outcome",
                 error:Safe}.

json_safe(Value, Safe) :-
    var(Value),
    !,
    term_string(Value, Text, [quoted(true), numbervars(true)]),
    Safe = _{type:"prolog_variable", value:Text}.
json_safe(Value, Safe) :-
    is_dict(Value),
    !,
    dict_pairs(Value, Tag, Pairs0),
    maplist(json_safe_pair, Pairs0, Pairs),
    safe_tag(Tag, TagSafe),
    (   TagSafe == null
    ->  dict_pairs(Safe, json, Pairs)
    ;   dict_pairs(Safe0, json, Pairs),
        put_dict('_tag', Safe0, TagSafe, Safe)
    ).
json_safe(Value, Safe) :-
    is_list(Value),
    !,
    maplist(json_safe, Value, Safe).
json_safe(Value, Value) :-
    string(Value),
    !.
json_safe(Value, Value) :-
    number(Value),
    !.
json_safe(true, true) :- !.
json_safe(false, false) :- !.
json_safe(null, null) :- !.
json_safe(Value, Safe) :-
    atom(Value),
    !,
    atom_string(Value, Safe).
json_safe(Value, Safe) :-
    compound(Value),
    !,
    Value =.. [Functor|Args],
    atom_string(Functor, FunctorText),
    maplist(json_safe, Args, SafeArgs),
    Safe = _{type:"prolog_term",
             functor:FunctorText,
             args:SafeArgs}.
json_safe(Value, Safe) :-
    term_string(Value, Safe, [quoted(true), numbervars(true)]).

json_safe_pair(Key-Value0, Key-Value) :-
    json_safe(Value0, Value).

safe_tag(Tag, null) :- var(Tag), !.
safe_tag(Tag, Safe) :- atom(Tag), !, atom_string(Tag, Safe).
safe_tag(Tag, Safe) :- text_value(Tag, Safe).

require_text(Dict, Key, Text) :-
    (   get_dict(Key, Dict, Value),
        text_value(Value, Text),
        Text \== ""
    ->  true
    ;   throw(error(domain_error(nonempty_text_field, Key), _))
    ).

text_value(Value, Value) :- string(Value), !.
text_value(Value, Text) :- atom(Value), !, atom_string(Value, Text).
text_value(Value, _) :-
    throw(error(type_error(text, Value), _)).

write_bridge_exception(Exception) :-
    json_safe(Exception, Safe),
    write_json(_{ok:false,
                 kind:"bridge_error",
                 error:Safe}).

write_json(Dict) :-
    json_write_dict(current_output, Dict, [width(0)]),
    nl,
    flush_output(current_output).
