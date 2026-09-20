:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_zara_expert).
test(deployment_services_are_preserved) :-
    services(['zara-server.service', 'zara-wake.service', 'zara-desktop.service']).
test(home_manager_owns_deployment) :- deployment_owner(home_manager, 'unseen@flake').
test(real_voice_requires_real_speech) :-
    voice_stage(voice_input, requires_real_speech_not_tone_fixtures).
test(provider_preservation_invariant) :-
    tts_invariant(preserve_configured_provider_without_user_approval).
:- end_tests(dotfiles_zara_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
