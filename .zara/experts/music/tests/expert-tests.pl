:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_music_expert).

test(identity_and_zero_model_policy) :-
    expert_id('zara:expert/music'),
    provider_policy(disabled),
    max_model_calls(0),
    model_calls(0).

test(playback_actions_are_closed) :-
    forall(member(Action, [play, pause, play_pause, next, previous, stop, seek, volume]),
           playback_action(Action)).

test(arbitrary_playback_action_is_rejected, [fail]) :-
    playback_action(delete_library).

test(now_playing_routes_to_media_context) :-
    music_decision(now_playing,
        music_decision(
            expert('zara:expert/music'),
            request(now_playing),
            tool('media.context'),
            effect(observation),
            authority(zara_core),
            provider_policy(disabled),
            model_calls(0)
        )).

test(playback_routes_to_canonical_media_tool) :-
    music_decision(playback(next),
        music_decision(
            expert('zara:expert/music'),
            request(playback(next)),
            tool('media.playback.control'),
            effect(media_mutation),
            authority(zara_core),
            provider_policy(disabled),
            model_calls(0)
        )).

test(discussion_requires_fresh_track_context) :-
    discussion_requirement(discuss_current, Requirements),
    memberchk(fresh_media_context, Requirements),
    memberchk(track_identity, Requirements),
    memberchk(artist_identity, Requirements).

test(recommendation_never_implies_playback) :-
    recommendation_requirement(recommend_from_current, Requirements),
    memberchk(no_implicit_playback_effect, Requirements).

:- end_tests(dotfiles_music_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
