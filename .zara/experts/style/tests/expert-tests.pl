:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(style_expert).

test(identity_and_zero_model_policy_are_exact) :-
    expert_id('zara:expert/style'),
    source_owner('lost-rob0t/dotfiles#292'),
    upstream_contract('lost-rob0t/prolog-rlm#504'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(nix_rules_are_closed_project_scoped_upstream_rule_dicts) :-
    style_rules('workspace:dotfiles', 17, nix, Rules),
    Rules \== [],
    forall(member(Rule, Rules),
           ( is_dict(Rule, style_rule),
             dict_keys(Rule, Keys),
             Keys == [autofix,check,id,language,overrides,preferred,project_generation,project_id,provenance,revision,scope],
             Rule.project_id == 'workspace:dotfiles',
             Rule.project_generation == 17,
             Rule.autofix == none,
             Rule.overrides == []
           )),
    member(ProjectRuntime, Rules),
    ProjectRuntime.scope == project_global,
    ProjectRuntime.language == any,
    ProjectRuntime.check == 'expert_runtime.hidden_model_fallback',
    ProjectRuntime.preferred == false,
    member(NixFormatter, Rules),
    NixFormatter.scope == project_language,
    NixFormatter.language == nix,
    NixFormatter.check == 'formatting.formatter',
    NixFormatter.preferred == 'nixfmt-rfc-style'.

test(bash_rules_preserve_canonical_language_revision_and_provenance) :-
    style_rules('workspace:dotfiles', 9, bash, Rules),
    member(Rule, Rules),
    Rule.scope == project_language,
    Rule.language == bash,
    Rule.revision == 'dotfiles-bash-style-v1',
    Rule.provenance.source == '.zara/experts/bash/kb/expert.pl',
    Rule.provenance.kind == canonical_expert.

test(rule_ids_are_unique) :-
    style_rules('workspace:dotfiles', 1, nix, Rules),
    findall(Id, (member(Rule, Rules), Id = Rule.id), Ids),
    sort(Ids, Unique),
    same_length(Ids, Unique).

test(unsupported_language_fails_closed, [fail]) :-
    style_rules('workspace:dotfiles', 1, python, _).

test(invalid_project_generation_fails_closed, [fail]) :-
    style_rules('workspace:dotfiles', -1, nix, _).

test(empty_project_identity_fails_closed, [fail]) :-
    style_rules('', 1, nix, _).

:- end_tests(style_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
