% Durable dotfiles knowledge base.
% Load every focused KB file from here:
%   ?- consult("index").
%
% Raw per-run execution state lives in .prolog/runs/ (untracked).

:- consult("home_manager_ownership").
:- consult("nixpkgs_deprecations").
:- consult("literate_sync").
:- consult("forgejo_ci").
:- consult("opencode_commands").
:- consult("opencode_worker").
:- consult("emacs_entrypoint").
:- consult("emacs_org_agenda").
:- consult("qtile_widget_api").
:- consult("zara_deployment").
:- consult("zara_voice_validation").
:- consult("zara_pairing").
