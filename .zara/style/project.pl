:- module(dotfiles_project_style,
    [ style_revision/1,
      style_rule/3,
      style_provenance/4
    ]).

style_revision('dotfiles-project-style-v1').

% Project-wide rules are inert symbolic policy. They narrow behavior only and do
% not grant filesystem, shell, network, model, or other execution authority.
style_rule(source_ownership, edit_generated_artifact_directly, false).
style_rule(verification, success_requires_fresh_postcondition, true).
style_rule(expert_runtime, hidden_model_fallback, false).
style_rule(expert_runtime, providers_required, false).

style_provenance(source_ownership,
                 '.zara/experts/AGENTS.md',
                 authored,
                 'dotfiles-project-style-v1').
style_provenance(verification,
                 '.zara/experts/AGENTS.md',
                 authored,
                 'dotfiles-project-style-v1').
style_provenance(expert_runtime,
                 '.zara/experts/AGENTS.md',
                 authored,
                 'dotfiles-project-style-v1').
