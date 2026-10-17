owner(zara_symbolic_chat_expert, '.zara/experts/symbolic-chat').
deployment_path(zara_symbolic_chat_expert, 'nix build .#zara-expert-library').
invariant(zara_symbolic_chat_expert, provider_policy(disabled)).
invariant(zara_symbolic_chat_expert, max_model_calls(0)).
failure_mode(zara_symbolic_chat_expert, ambiguous_topic_query_fails_closed).
failure_mode(zara_symbolic_chat_expert, unsupported_topic_query_fails_closed).
