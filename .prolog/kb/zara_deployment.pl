zara_deployment_owner(home_manager, 'unseen@flake').
zara_deployment_input(zara, 'flake.lock').
zara_deployment_services(['zara-server.service', 'zara-wake.service', 'zara-desktop.service']).
zara_deployment_invariant(preserve_other_dirty_lock_inputs).
zara_deployment_invariant(compare_service_execstart_to_evaluated_package).
zara_startup_observation(wake_may_retry_before_daemon_ready).
