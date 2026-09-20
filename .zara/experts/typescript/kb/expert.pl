:- module(dotfiles_typescript_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      module_system/1,
      jsx_variant/1,
      compiler_config_role/1,
      project_metadata_role/1,
      repair_verification/1,
      model_calls/1
    ]).

expert_id('zara:expert/typescript').
upstream_contract('lost-rob0t/prolog-rlm#500').

accepts_extension(ts).
accepts_extension(tsx).
accepts_extension(mts).
accepts_extension(cts).

supports_semantic(parse).
supports_semantic(module_reasoning).
supports_semantic(typecheck).
supports_semantic(type_only_imports).
supports_semantic(generics).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

module_system(esm).
module_system(commonjs).
jsx_variant(tsx).

% tsconfig/project facts refine deterministic compiler semantics, never authority.
compiler_config_role(observation_only).
project_metadata_role(observation_only).

repair_verification(reparse_and_typecheck).
model_calls(0).
