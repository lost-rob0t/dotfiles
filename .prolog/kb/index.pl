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

% ZARA-CREW/1: append the versioned contract without replacing prior KB imports.
:- use_module(crew_protocol_v1).
