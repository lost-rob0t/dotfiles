zara_deployment_owner(home_manager, 'unseen@flake').
zara_deployment_input(zara, 'flake.lock').
zara_deployment_services(['zara-server.service', 'zara-wake.service', 'zara-desktop.service']).
zara_deployment_invariant(preserve_other_dirty_lock_inputs).
zara_deployment_invariant(compare_service_execstart_to_evaluated_package).
zara_startup_observation(wake_may_retry_before_daemon_ready).
zara_profile('unseen@flake', local_ipc, cpu_voice).
zara_profile('unseen@desktop', disabled, no_voice_runtime).
zara_profile('unseen@hunter02', disabled, no_voice_runtime).
zara_starintel_tool(starintel_search, 'https://ingest.starintel.actor/api/v1/search', read_only).
zara_starintel_tool_limit(public_search_result_quality, 'starintel-server Forgejo issue 20').
