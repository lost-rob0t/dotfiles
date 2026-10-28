% Durable dotfiles knowledge base.
% Load every focused KB file from here:
%   ?- consult("index").
%
% Raw per-run execution state lives in .prolog/runs/ (untracked).

:- multifile invariant/2.
:- multifile repair_recipe/2.
:- multifile failure_mode/2.
:- multifile infra_quirk/2.
:- multifile owner/2.
:- multifile deployment_path/2.

:- consult("home_manager_ownership").
:- consult("starintel_admin").
:- consult("nixpkgs_deprecations").
:- consult("literate_sync").
:- consult("gpt_todos_sync").
:- consult("forgejo_ci").
:- consult("opencode_commands").
:- consult("opencode_worker").
:- consult("emacs_entrypoint").
:- consult("zara_deployment").
:- consult("zara_voice_validation").
:- consult("zara_pairing").

:- consult('qtile_monitors.pl').
