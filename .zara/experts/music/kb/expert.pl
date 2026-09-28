:- module(dotfiles_music_expert,
    [ expert_id/1,
      supports_request/1,
      playback_action/1,
      tool_route/2,
      effect_class/2,
      authority_owner/1,
      discussion_requirement/2,
      recommendation_requirement/2,
      music_decision/2,
      provider_policy/1,
      max_model_calls/1,
      model_calls/1
    ]).

expert_id('zara:expert/music').

playback_action(play).
playback_action(pause).
playback_action(play_pause).
playback_action(next).
playback_action(previous).
playback_action(stop).
playback_action(seek).
playback_action(volume).

supports_request(now_playing).
supports_request(players).
supports_request(playback(Action)) :- playback_action(Action).
supports_request(discuss_current).
supports_request(recommend_from_current).

tool_route(now_playing, 'media.context').
tool_route(players, 'media.players').
tool_route(playback(_), 'media.playback.control').
tool_route(discuss_current, 'media.context').
tool_route(recommend_from_current, 'media.like_this').

effect_class(now_playing, observation).
effect_class(players, observation).
effect_class(playback(_), media_mutation).
effect_class(discuss_current, observation_then_conversation).
effect_class(recommend_from_current, observation_then_recommendation).

% This expert classifies and routes. Zara Core remains the authority owner for
% effects, approvals, budgets, and any later model handoff.
authority_owner(zara_core).

discussion_requirement(discuss_current,
    [fresh_media_context, track_identity, artist_identity, player_identity]).
recommendation_requirement(recommend_from_current,
    [fresh_media_context, explicit_seed, no_implicit_playback_effect]).

music_decision(Request,
    music_decision(
        expert('zara:expert/music'),
        request(Request),
        tool(Tool),
        effect(Effect),
        authority(zara_core),
        provider_policy(disabled),
        model_calls(0)
    )) :-
    supports_request(Request),
    tool_route(Request, Tool),
    effect_class(Request, Effect).

% The symbolic expert itself never calls a provider. Discussion or
% recommendation may be handed to an outer Zara runtime only after the
% deterministic media context has been observed.
provider_policy(disabled).
max_model_calls(0).
model_calls(0).
