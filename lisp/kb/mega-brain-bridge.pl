:- module(mega_brain_bridge, [main/0]).

:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(solution_sequences)).

:- dynamic configured_source/1.
:- dynamic loaded_source/1.

main :-
    current_prolog_flag(argv, Sources0),
    maplist(normalize_source, Sources0, Sources),
    maplist(assert_configured, Sources),
    reload_sources,
    serve.

assert_configured(Path) :-
    ( configured_source(Path) -> true ; assertz(configured_source(Path)) ).

normalize_source(Path0, Path) :-
    absolute_file_name(Path0, Path,
        [ access(read),
          file_errors(fail),
          solutions(first)
        ]),
    file_name_extension(_, pl, Path).

reload_sources :-
    forall(retract(loaded_source(Path)),
           catch(unload_file(Path), _, true)),
    forall(configured_source(Path), load_source(Path)).

load_source(Path) :-
    load_files(Path, [if(true), silent(true)]),
    assertz(loaded_source(Path)).

serve :-
    read_line_to_string(user_input, Line),
    ( Line == end_of_file
    -> true
    ; handle_line(Line, Response),
      json_write_dict(current_output, Response, [width(0)]),
      nl,
      flush_output,
      serve
    ).

handle_line(Line, Response) :-
    catch(
        ( atom_string(Atom, Line),
          atom_json_dict(Atom, Request, []),
          request_id(Request, Id),
          dispatch(Request, Result),
          Response = _{id:Id, ok:true, result:Result}
        ),
        Error,
        error_response(Line, Error, Response)
    ).

request_id(Request, Id) :-
    ( get_dict(id, Request, Id0) -> Id = Id0 ; Id = null ).

error_response(Line, Error, Response) :-
    catch(
        ( atom_string(Atom, Line),
          atom_json_dict(Atom, Request, []),
          request_id(Request, Id)
        ),
        _,
        Id = null
    ),
    message_to_string(Error, Message),
    Response = _{id:Id, ok:false, error:_{message:Message}}.

dispatch(Request, Result) :-
    require_operation(Request, Operation),
    dispatch_operation(Operation, Request, Result).

require_operation(Request, Operation) :-
    get_dict(operation, Request, Value),
    string(Value),
    atom_string(Operation, Value).

dispatch_operation(ping, _, _{bridge:"MEGA-BRAIN/1", status:"ready"}).
dispatch_operation(status, _, Result) :-
    findall(Path, loaded_source(Path), Sources),
    findall(_{module:Module, predicate:Name, arity:Arity},
            api_predicate(Module, Name, Arity),
            Predicates),
    Result = _{
        bridge:"MEGA-BRAIN/1",
        status:"ready",
        sources:Sources,
        predicates:Predicates
    }.
dispatch_operation(reset, _, _{status:"reset"}) :-
    reload_sources.
dispatch_operation(query, Request, Result) :-
    query_request(Request, Result).
dispatch_operation(Operation, _, _) :-
    domain_error(mega_brain_operation, Operation).

query_request(Request, Result) :-
    request_text(Request, module, "user", ModuleString),
    request_text(Request, predicate, "", PredicateString),
    safe_identifier(ModuleString),
    safe_identifier(PredicateString),
    atom_string(Module, ModuleString),
    atom_string(Name, PredicateString),
    request_args(Request, JsonArgs),
    length(JsonArgs, Arity),
    api_predicate(Module, Name, Arity),
    json_args(JsonArgs, Args, Bindings),
    Head =.. [Name|Args],
    request_limit(Request, Limit),
    findnsols(Limit, Bindings, call(Module:Head), Rows0),
    maplist(binding_row_json, Rows0, Rows),
    Result = _{
        module:ModuleString,
        predicate:PredicateString,
        arity:Arity,
        rows:Rows,
        count:Rows.length
    }.

request_text(Request, Key, Default, Value) :-
    ( get_dict(Key, Request, Raw) -> true ; Raw = Default ),
    string(Raw),
    Value = Raw.

request_args(Request, Args) :-
    ( get_dict(args, Request, Raw) -> true ; Raw = [] ),
    is_list(Raw),
    length(Raw, Length),
    Length =< 16,
    Args = Raw.

request_limit(Request, Limit) :-
    ( get_dict(limit, Request, Raw) -> true ; Raw = 50 ),
    integer(Raw),
    between(1, 100, Raw),
    Limit = Raw.

safe_identifier(String) :-
    string(String),
    re_match("^[a-z][A-Za-z0-9_]*$", String).

json_args([], [], []).
json_args([Json|Rest], [Term|Terms], Bindings) :-
    json_arg(Json, Term, Here),
    json_args(Rest, Terms, Tail),
    append(Here, Tail, Bindings).

json_arg(Json, Term, [Name-Term]) :-
    is_dict(Json),
    get_dict(var, Json, Name),
    string(Name),
    re_match("^[A-Za-z][A-Za-z0-9_]*$", Name),
    !.
json_arg(Json, Term, []) :-
    is_dict(Json),
    get_dict(functor, Json, FunctorString),
    get_dict(args, Json, JsonArgs),
    safe_identifier(FunctorString),
    atom_string(Functor, FunctorString),
    is_list(JsonArgs),
    maplist(json_ground_term, JsonArgs, Args),
    length(Args, Arity),
    Arity =< 12,
    Term =.. [Functor|Args],
    !.
json_arg(Json, Term, []) :-
    is_list(Json),
    maplist(json_ground_term, Json, Term),
    !.
json_arg(Json, Json, []) :-
    json_scalar(Json).

json_ground_term(Json, Term) :-
    ( is_dict(Json)
    -> get_dict(functor, Json, FunctorString),
       get_dict(args, Json, JsonArgs),
       safe_identifier(FunctorString),
       atom_string(Functor, FunctorString),
       is_list(JsonArgs),
       maplist(json_ground_term, JsonArgs, Args),
       length(Args, Arity),
       Arity =< 12,
       Term =.. [Functor|Args]
    ; is_list(Json)
    -> maplist(json_ground_term, Json, Term)
    ; json_scalar(Json)
    -> Term = Json
    ).

json_scalar(Value) :- string(Value), !.
json_scalar(Value) :- number(Value), !.
json_scalar(true).
json_scalar(false).
json_scalar(null).

binding_row_json(Pairs, Dict) :-
    maplist(binding_json_pair, Pairs, JsonPairs),
    dict_pairs(Dict, bindings, JsonPairs).

binding_json_pair(Name-Value, Key-Json) :-
    atom_string(Key, Name),
    term_json(Value, Json).

term_json(Value, null) :- var(Value), !.
term_json(Value, Value) :- json_scalar(Value), !.
term_json(Value, Json) :-
    is_list(Value),
    !,
    maplist(term_json, Value, Json).
term_json(Value, _{functor:FunctorString, args:ArgsJson}) :-
    compound(Value),
    Value =.. [Functor|Args],
    atom_string(Functor, FunctorString),
    maplist(term_json, Args, ArgsJson),
    !.
term_json(Value, String) :-
    term_string(Value, String, [quoted(true), numbervars(true)]).

api_predicate(user, roam_file, 9).
api_predicate(user, roam_tag, 2).
api_predicate(user, roam_heading, 5).
api_predicate(user, roam_link, 2).

api_predicate(dotfiles_inventory_expert, expert_id, 1).
api_predicate(dotfiles_inventory_expert, model_calls, 1).
api_predicate(dotfiles_inventory_expert, canonical_store, 1).
api_predicate(dotfiles_inventory_expert, inventory_delta, 3).
api_predicate(dotfiles_inventory_expert, event_kind, 1).
api_predicate(dotfiles_inventory_expert, current_quantity, 2).
api_predicate(dotfiles_inventory_expert, reorder_needed, 2).
api_predicate(dotfiles_inventory_expert, recipe_missing, 3).
api_predicate(dotfiles_inventory_expert, recipe_available, 2).
api_predicate(dotfiles_inventory_expert, ai_recipe_requires_provenance, 1).
api_predicate(dotfiles_inventory_expert, food_table_columns, 1).
api_predicate(dotfiles_inventory_expert, recipe_table_columns, 1).

:- initialization(main, main).
