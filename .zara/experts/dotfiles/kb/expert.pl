:- module(dotfiles_expert,
    [ expert_id/1,
      source_owner/1,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1,
      kb_source/5,
      classify_path/4,
      nix_path/1,
      bash_path/1,
      ownership/3,
      home_manager_owned_path/1,
      specialist_for/4,
      explain_decision/3,
      style_source/4,
      style_revision/2
    ]).

% DotfilesExpert is project-domain reasoning only. Zara Core owns registry,
% lifecycle, budgets, authority and effects; zara-plugins owns product adapters.
expert_id('zara:expert/dotfiles').
source_owner('lost-rob0t/dotfiles#282').
provider_policy(disabled).
max_model_calls(0).
model_calls(0).

% Reuse reviewed durable project memory instead of copying it into this package.
:- ensure_loaded('../../../../.prolog/kb/home_manager_ownership.pl').
:- ensure_loaded('../../../../.prolog/kb/literate_sync.pl').

% source id, repo-relative path, exact source revision, scope, authored/generated.
kb_source(home_manager_ownership,
          '.prolog/kb/home_manager_ownership.pl',
          'git-blob:890a27a75618c620acc18d47c15c4c98ae0d5b9e',
          project,
          authored).
kb_source(literate_sync,
          '.prolog/kb/literate_sync.pl',
          'git-blob:8c3482cef3d622646d590e257e3c53eb3f17d9c0',
          project,
          authored).

% Classification is deliberately conservative. Unknown paths have no clause and
% become typed unknown/unsupported downstream; they never trigger a model fallback.
classify_path(Path, nix, 'zara:expert/nix',
              'path-extension:.nix') :-
    atom(Path),
    file_name_extension(_, nix, Path).
classify_path(Path, bash, 'zara:expert/bash',
              'path-extension:.sh') :-
    atom(Path),
    file_name_extension(_, sh, Path).
classify_path(Path, bash, 'zara:expert/bash',
              'path-extension:.bash') :-
    atom(Path),
    file_name_extension(_, bash, Path).
classify_path(Path, bash, 'zara:expert/bash',
              'shell-startup-basename') :-
    atom(Path),
    file_base_name(Path, Base),
    memberchk(Base, ['.bashrc', '.bash_profile', '.profile']).

% Unary adapter predicates intentionally expose only fixed, host-registered
% questions. Invocation payloads cannot select a predicate or manufacture a goal.
nix_path(Path) :- classify_path(Path, nix, 'zara:expert/nix', _).
bash_path(Path) :- classify_path(Path, bash, 'zara:expert/bash', _).

ownership(Path, home_manager,
          'kb:.prolog/kb/home_manager_ownership.pl#hm_owns_path/1') :-
    atom(Path),
    hm_owns_path(Path).

home_manager_owned_path(Path) :- ownership(Path, home_manager, _).

specialist_for(Path, Language, ExpertId, Reason) :-
    classify_path(Path, Language, ExpertId, Evidence),
    Reason = delegation(Language, ExpertId, Evidence).

explain_decision(Path, specialist, Explanation) :-
    specialist_for(Path, Language, ExpertId, Reason),
    Explanation = explanation(
        decision(specialist),
        path(Path),
        language(Language),
        expert(ExpertId),
        evidence(Reason)).
explain_decision(Path, ownership, Explanation) :-
    ownership(Path, Owner, Evidence),
    Explanation = explanation(
        decision(ownership),
        path(Path),
        owner(Owner),
        evidence(Evidence)).

% Style files are inert project-owned rule sources. The adapter resolves them
% through the authenticated workspace generation; these facts grant no authority.
style_source(project, any,
             '.zara/style/project.pl',
             'dotfiles-project-style-v1').
style_source(project_language, nix,
             '.zara/style/languages/nix.pl',
             'dotfiles-nix-style-v1').
style_source(project_language, bash,
             '.zara/style/languages/bash.pl',
             'dotfiles-bash-style-v1').

style_revision(project, 'dotfiles-project-style-v1').
style_revision(nix, 'dotfiles-nix-style-v1').
style_revision(bash, 'dotfiles-bash-style-v1').
