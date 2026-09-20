:- module(dotfiles_javascript_expert,
    [ expert_id/1,
      upstream_contract/1,
      accepts_extension/1,
      supports_semantic/1,
      module_system/1,
      jsx_variant/1,
      project_metadata_role/1,
      repair_verification/1,
      model_calls/1
    ]).

expert_id('zara:expert/javascript').
upstream_contract('lost-rob0t/prolog-rlm#500').

accepts_extension(js).
accepts_extension(jsx).
accepts_extension(mjs).
accepts_extension(cjs).

supports_semantic(parse).
supports_semantic(module_reasoning).
supports_semantic(diagnose).
supports_semantic(style).
supports_semantic(repair_verify).

module_system(esm).
module_system(commonjs).
jsx_variant(jsx).

% Project metadata may refine style/module interpretation, never grant authority.
project_metadata_role(observation_only).

repair_verification(reparse).
model_calls(0).
