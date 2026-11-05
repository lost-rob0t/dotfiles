% pi coding-agent extension API knowledge (pi 1.4.x).
% Verified 2026-11-05 while building pi/agent/extensions/rlm-ops.

% Tool execution events: tool_execution_end carries toolCallId/toolName/
% result/isError but NOT the call arguments; tool_call carries input.
invariant(observation_args_source,
    'capture call inputs in a tool_call handler keyed by toolCallId; tool_execution_end no longer carries argument data').

% Background events: sendUserMessage triggers a run when idle and throws
% while streaming unless deliverAs is set (followUp queues for next turn).
workflow(bg_finish_events,
    'deliver background completion events with sendUserMessage when ctx.isIdle(), else sendUserMessage(text, {deliverAs: "followUp"})').

% Headless children (pi --print) load USER extensions, so extension-based
% sub-agent tooling must be deployed to ~/.pi/agent/extensions to reach
% children; -e on the parent does not propagate.
deployment_path(pi_user_extensions,
    '~/.pi/agent/extensions -> ../../dotfiles/pi/agent/extensions (children spawned with --print load user extensions and inherit the shared toolset)').

% setActiveTools replaces the whole active set; filtering an early
% getActiveTools() snapshot can drop tools and break child sessions.
infra_quirk(setactivetools_replaces,
    'pi.setActiveTools replaces the active set; avoid narrowing at session_start — register tools with the desired default exposure instead').

% Node >= 22.6 strips TS types but rejects parameter properties
% (constructor(private x)); pi jiti accepts them, native node does not.
infra_quirk(node_strip_types_no_param_props,
    'extension TS must avoid constructor parameter properties for node-native strip-types testing (node >= 24 recommended)').
