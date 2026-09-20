:- use_module(library(plunit)).
:- use_module('../kb/expert').
:- use_module('../../../style/project', []).
:- use_module('../../../style/languages/nix', []).
:- use_module('../../../style/languages/bash', []).

:- begin_tests(dotfiles_expert).

test(identity_and_zero_model_policy_are_exact) :-
    expert_id('zara:expert/dotfiles'),
    source_owner('lost-rob0t/dotfiles#282'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(provenance_index_reuses_durable_kb_revision) :-
    kb_source(home_manager_ownership,
              '.prolog/kb/home_manager_ownership.pl',
              'git-blob:890a27a75618c620acc18d47c15c4c98ae0d5b9e',
              project,
              authored).

test(existing_kb_answers_real_ownership_query) :-
    ownership('/.config/systemd/user/zara-server.service',
              home_manager,
              'kb:.prolog/kb/home_manager_ownership.pl#hm_owns_path/1').

test(nix_path_delegates_to_registered_nix_identity) :-
    specialist_for('flake.nix', nix, 'zara:expert/nix',
                   delegation(nix, 'zara:expert/nix', 'path-extension:.nix')).

test(bash_path_delegates_to_registered_bash_identity) :-
    specialist_for('bin/deploy.sh', bash, 'zara:expert/bash',
                   delegation(bash, 'zara:expert/bash', 'path-extension:.sh')).

test(shell_startup_file_is_bash) :-
    classify_path('home/.bashrc', bash, 'zara:expert/bash',
                  'shell-startup-basename').

test(unknown_path_fails_closed, [fail]) :-
    classify_path('README.md', _, _, _).

test(explanation_keeps_delegation_evidence) :-
    explain_decision('flake.nix', specialist,
                     explanation(decision(specialist),
                                 path('flake.nix'),
                                 language(nix),
                                 expert('zara:expert/nix'),
                                 evidence(delegation(nix,
                                                     'zara:expert/nix',
                                                     'path-extension:.nix')))).

test(project_style_source_is_versioned) :-
    style_source(project, any,
                 '.zara/style/project.pl',
                 'dotfiles-project-style-v1'),
    dotfiles_project_style:style_revision('dotfiles-project-style-v1'),
    dotfiles_project_style:style_rule(verification,
                                      success_requires_fresh_postcondition,
                                      true).

test(nix_style_is_project_language_scoped_and_provenanced) :-
    style_source(project_language, nix,
                 '.zara/style/languages/nix.pl',
                 'dotfiles-nix-style-v1'),
    dotfiles_nix_style:style_rule(formatting, formatter, 'nixfmt-rfc-style'),
    dotfiles_nix_style:style_provenance(formatting,
                                        '.prolog/kb/literate_sync.pl',
                                        durable_kb,
                                        'dotfiles-nix-style-v1').

test(bash_style_keeps_source_inert) :-
    style_source(project_language, bash,
                 '.zara/style/languages/bash.pl',
                 'dotfiles-bash-style-v1'),
    dotfiles_bash_style:style_rule(inspection, source_is_inert_data, true),
    dotfiles_bash_style:style_rule(inspection, source_before_analysis, false).

:- end_tests(dotfiles_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
