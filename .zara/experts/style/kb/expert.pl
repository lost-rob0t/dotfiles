:- module(dotfiles_style_expert,
    [ expert_id/1,
      source_owner/1,
      upstream_contract/1,
      supports_operation/1,
      supported_language/1,
      style_source/4,
      style_revision/2,
      style_rules/4,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

:- use_module('../../../style/project', []).
:- use_module('../../../style/languages/nix', []).
:- use_module('../../../style/languages/bash', []).

expert_id('zara:expert/style').
source_owner('lost-rob0t/dotfiles#292').
upstream_contract('lost-rob0t/prolog-rlm#504').

supports_operation(resolve).
supports_operation(explain).

supported_language(nix).
supported_language(bash).

style_source(project_global, any,
             '.zara/style/project.pl', Revision) :-
    dotfiles_project_style:style_revision(Revision).
style_source(project_language, nix,
             '.zara/style/languages/nix.pl', Revision) :-
    dotfiles_nix_style:style_revision(Revision).
style_source(project_language, bash,
             '.zara/style/languages/bash.pl', Revision) :-
    dotfiles_bash_style:style_revision(Revision).

style_revision(project, Revision) :-
    dotfiles_project_style:style_revision(Revision).
style_revision(nix, Revision) :-
    dotfiles_nix_style:style_revision(Revision).
style_revision(bash, Revision) :-
    dotfiles_bash_style:style_revision(Revision).

% Produce only inert, closed style-rule data. Precedence, conflict handling and
% resolution remain owned by Prolog-RLM style_overlay_resolve/3. Project identity
% and generation come from Zara's caller-owned InvocationFence and are copied into
% project-bound rules so stale generations fail in the canonical resolver.
style_rules(ProjectId, Generation, Language, Rules) :-
    valid_project_identity(ProjectId),
    integer(Generation),
    Generation >= 0,
    supported_language(Language),
    findall(Rule,
            project_rule(ProjectId, Generation, Rule),
            ProjectRules),
    findall(Rule,
            language_rule(ProjectId, Generation, Language, Rule),
            LanguageRules),
    append(ProjectRules, LanguageRules, Rules).

project_rule(ProjectId, Generation, Rule) :-
    dotfiles_project_style:style_revision(Revision),
    dotfiles_project_style:style_rule(Category, Key, Preferred),
    dotfiles_project_style:style_provenance(Category, Source, Kind, Revision),
    rule_identity(project, Category, Key, Id),
    rule_check(Category, Key, Check),
    Rule = style_rule{
        id:Id,
        scope:project_global,
        language:any,
        project_id:ProjectId,
        project_generation:Generation,
        check:Check,
        preferred:Preferred,
        autofix:none,
        provenance:style_provenance{
            source:Source,
            kind:Kind,
            scope:project_global
        },
        revision:Revision,
        overrides:[]
    }.

language_rule(ProjectId, Generation, nix, Rule) :-
    dotfiles_nix_style:style_revision(Revision),
    dotfiles_nix_style:style_rule(Category, Key, Preferred),
    dotfiles_nix_style:style_provenance(Category, Source, Kind, Revision),
    language_rule_dict(ProjectId, Generation, nix, Category, Key, Preferred,
                       Source, Kind, Revision, Rule).
language_rule(ProjectId, Generation, bash, Rule) :-
    dotfiles_bash_style:style_revision(Revision),
    dotfiles_bash_style:style_rule(Category, Key, Preferred),
    dotfiles_bash_style:style_provenance(Category, Source, Kind, Revision),
    language_rule_dict(ProjectId, Generation, bash, Category, Key, Preferred,
                       Source, Kind, Revision, Rule).

language_rule_dict(ProjectId, Generation, Language, Category, Key, Preferred,
                   Source, Kind, Revision, Rule) :-
    rule_identity(Language, Category, Key, Id),
    rule_check(Category, Key, Check),
    Rule = style_rule{
        id:Id,
        scope:project_language,
        language:Language,
        project_id:ProjectId,
        project_generation:Generation,
        check:Check,
        preferred:Preferred,
        autofix:none,
        provenance:style_provenance{
            source:Source,
            kind:Kind,
            scope:project_language
        },
        revision:Revision,
        overrides:[]
    }.

rule_identity(Scope, Category, Key, Id) :-
    atomic_list_concat([dotfiles, Scope, Category, Key], '.', Id).

rule_check(Category, Key, Check) :-
    atomic_list_concat([Category, Key], '.', Check).

valid_project_identity(ProjectId) :-
    atomic(ProjectId),
    ProjectId \== ''.

provider_policy(disabled).
max_model_calls(0).
model_calls(0).
