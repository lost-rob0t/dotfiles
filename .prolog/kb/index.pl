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
:- consult("nix_flake_pins").
:- consult("opencode_worker").
