:- module(dotfiles_nim_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      evidence_role/2,
      compiler_probe/2,
      source_execution_policy/1,
      generation_current/2,
      repair_verification/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/nim').
upstream_contract('lost-rob0t/prolog-rlm#499').

accepts_extension(nim).
accepts_extension(nims).

supports_semantic(parse).
supports_semantic(compile_check).
supports_semantic(indentation).
supports_semantic(proc_reasoning).
supports_semantic(func_reasoning).
supports_semantic(template_reasoning).
supports_semantic(macro_reasoning).
supports_semantic(type_reasoning).
supports_semantic(import_reasoning).
supports_semantic(generics).
supports_semantic(pragmas).
supports_semantic(style).
supports_semantic(repair_verify).

% Compiler diagnostics are typed observations; source is never executed by inspection.
evidence_role(parser, observation_only).
evidence_role(compiler, observation_only).
compiler_probe(nim_check, typed_argv).
source_execution_policy(never).

generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

repair_verification(reparse_and_nim_check).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
