:- module(dotfiles_zara_expert,
    [ deployment_owner/2, deployment_input/2, services/1, invariant/1,
      startup_observation/1, voice_stage/2, known_voice_failure/4, tts_invariant/1 ]).

:- ensure_loaded('../../../../.prolog/kb/zara_deployment.pl').
:- ensure_loaded('../../../../.prolog/kb/zara_voice_validation.pl').

deployment_owner(Owner, Profile) :- zara_deployment_owner(Owner, Profile).
deployment_input(Name, Source) :- zara_deployment_input(Name, Source).
services(Services) :- zara_deployment_services(Services).
invariant(Invariant) :- zara_deployment_invariant(Invariant).
startup_observation(Observation) :- zara_startup_observation(Observation).
voice_stage(Stage, Requirement) :- zara_e2e_stage(Stage, Requirement).
known_voice_failure(Commit, Backend, Location, Reason) :-
    zara_voice_failure(Commit, Backend, Location, Reason).
tts_invariant(Invariant) :- zara_tts_invariant(Invariant).
