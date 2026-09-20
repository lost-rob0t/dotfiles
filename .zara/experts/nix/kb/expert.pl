:- module(dotfiles_nix_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      inspection_build_policy/1,
      evaluation_policy/1,
      build_policy/1,
      parser_probe/2,
      project_metadata_role/1,
      repair_verification/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/nix').
upstream_contract('lost-rob0t/prolog-rlm#503').

accepts_extension(nix).

supports_semantic(parse).
supports_semantic(flake_reasoning).
supports_semantic(module_reasoning).
supports_semantic(home_manager_reasoning).
supports_semantic(option_provenance).
supports_semantic(derivation_reference).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

% Parsing and ownership inspection must never trigger evaluation or a build.
inspection_build_policy(never).
evaluation_policy(explicit_capability).
build_policy(explicit_capability).
parser_probe(nix_instantiate_parse, read_only).
project_metadata_role(observation_only).

% Repairs require fresh parse evidence and, when the operation used evaluation or
% module checking, a fresh bounded check through the host-owned capability path.
repair_verification(parse_then_eval_or_check).

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
