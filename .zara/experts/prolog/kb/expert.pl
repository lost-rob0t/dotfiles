:- module(dotfiles_prolog_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      evidence_role/2,
      source_execution_policy/1,
      unrestricted_call_policy/1,
      generation_current/2,
      repair_verification/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/prolog').
upstream_contract('lost-rob0t/prolog-rlm#495').

accepts_extension(pl).
accepts_extension(prolog).

supports_semantic(parse).
supports_semantic(module_reasoning).
supports_semantic(predicate_indicator_reasoning).
supports_semantic(import_export_reasoning).
supports_semantic(dcg_reasoning).
supports_semantic(singleton_diagnostics).
supports_semantic(undefined_predicate_diagnostics).
supports_semantic(arity_mismatch_diagnostics).
supports_semantic(style).
supports_semantic(repair_verify).

% Compiler/xref observations are evidence only; source text never becomes a goal.
evidence_role(parser, observation_only).
evidence_role(xref, observation_only).
evidence_role(compiler, observation_only).
source_execution_policy(never).
unrestricted_call_policy(denied).

% Any cached parser/xref/compiler evidence must match the current workspace generation.
generation_current(Expected, Observed) :-
    integer(Expected),
    integer(Observed),
    Expected >= 0,
    Observed >= 0,
    Expected =:= Observed.

% Repair success requires fresh parser/xref evidence after the candidate edit.
repair_verification(reparse_and_xref).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
