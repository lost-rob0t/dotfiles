:- module(zara_feature_lab_runtime, [main/0]).

:- use_module(library(http/json)).
:- initialization(main, main).

main :-
    catch(run, Error, fail_reply(Error)).

run :-
    json_read_dict(current_input, Request),
    require_text(Request, prolog_rlm_root, RlmRoot0),
    require_text(Request, expert_file, ExpertFile0),
    require_text(Request, expert_module, ExpertModuleText),
    readable_directory(RlmRoot0, RlmRoot),
    readable_file(ExpertFile0, ExpertFile),
    directory_file_path(RlmRoot, 'prolog/rlm.pl', RlmFile),
    readable_file(RlmFile, _),
    load_files(RlmFile, [silent(true)]),
    atom_string(ExpertModule, ExpertModuleText),
    load_files(ExpertFile, [silent(true), imports([])]),
    current_module(ExpertModule),
    rlm:rlm_version(Version0),
    rlm:rlm_ready,
    call(ExpertModule:expert_id(ExpertId0)),
    call(ExpertModule:provider_policy(disabled)),
    call(ExpertModule:max_model_calls(0)),
    call(ExpertModule:model_calls(0)),
    text_value(Version0, Version),
    text_value(ExpertId0, ExpertId),
    reply(_{
        ok:true,
        runtime:"prolog-rlm",
        runtime_version:Version,
        runtime_ready:true,
        symbolic_policy:"zero-model-expert-first",
        expert:ExpertId,
        provider_policy:"disabled",
        max_model_calls:0,
        model_calls:0
    }).

readable_directory(Path0, Path) :-
    absolute_file_name(Path0, Path,
        [file_type(directory), access(read), file_errors(fail)]),
    !.
readable_directory(Path, _) :-
    throw(error(existence_error(directory, Path), _)).

readable_file(Path0, Path) :-
    absolute_file_name(Path0, Path,
        [file_type(regular), access(read), file_errors(fail)]),
    !.
readable_file(Path, _) :-
    throw(error(existence_error(source_sink, Path), _)).

require_text(Dict, Key, Text) :-
    get_dict(Key, Dict, Value),
    text_value(Value, Text),
    Text \== "",
    !.
require_text(_, Key, _) :-
    throw(error(domain_error(nonempty_text_field, Key), _)).

text_value(Value, Value) :- string(Value), !.
text_value(Value, Text) :- atom(Value), !, atom_string(Value, Text).
text_value(Value, _) :- throw(error(type_error(text, Value), _)).

fail_reply(Error) :-
    term_string(Error, Message, [quoted(true), numbervars(true)]),
    reply(_{ok:false, error:Message}),
    halt(1).

reply(Dict) :-
    json_write_dict(current_output, Dict, [width(0)]),
    nl,
    flush_output(current_output).
