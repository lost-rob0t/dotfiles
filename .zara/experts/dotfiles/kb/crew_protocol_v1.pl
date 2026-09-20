:- module(dotfiles_crew_protocol_v1, []).
% Re-export, never copy the existing project KB into a second mutable store.
% Package/activation manifest remains owned by DotfilesExpert #282.
:- reexport('../../../../.prolog/kb/crew_protocol_v1').
