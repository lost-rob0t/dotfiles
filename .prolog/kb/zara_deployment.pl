zara_deployment_owner(home_manager, 'unseen@flake').
zara_deployment_input(zara, 'flake.lock').
zara_deployment_services(['zara-server.service', 'zara-pair.service', 'zara-wake.service', 'zara-desktop.service']).
zara_deployment_dependency(qwen3_tts_service, zara_tts_provider, 'http://127.0.0.1:7860').
zara_deployment_invariant(preserve_other_dirty_lock_inputs).
zara_deployment_invariant(compare_service_execstart_to_evaluated_package).
zara_startup_observation(wake_may_retry_before_daemon_ready).
zara_startup_observation(pair_may_fail_before_control_socket_exists).

%% Enabled-state check contract (verified 2026-10-20 re-enable on flake):
%% checks.zara-server and tests/zara-server.nix assert the flake profile
%% ExecStart carries --remote-endpoint tcp://0.0.0.0:6060 + --security-dir,
%% never a primary ' --endpoint ', and the shared desktop profile binds no
%% remote endpoint. checks.zara-all-plugins asserts every pinned registry
%% plugin lands in home.packages plus its discovery and <name>/lib files.
%% tests/zara-workflows.sh greps desktop home.nix for workflows.enable=true,
%% plugins enableAll=true, and qwen3-tts enable/autoStart=true markers.
zara_check_contract(enabled, ['zara-server', 'zara-all-plugins', 'zara-workflows.sh']).
zara_check_swap_rule(disabled_enabled_toggle,
    [flake_nix_check_name, tests/zara-server.nix, tests/zara-workflows.sh,
     docs/wiki/zara.org, docs/wiki/index.org]).
zara_profile_layout(shared_desktop, 'nix/home-manager/systems/desktop/home.nix').
zara_profile_layout(host_listener, 'nix/home-manager/systems/flake/home.nix').

%% Local development toggle (verified 2026-10-20): zara-dev off stops+disables
%% server/pair/desktop/wake (--with-tts adds qwen3-tts); disable is temporary,
%% next home-manager activation reconciles units back to declarative state.
zara_dev_toggle(off, units([server, pair, desktop, wake]), optional(tts)).
zara_dev_toggle(on, enable_now, activation_reconciles_disabled_units).

%% Non-fatal plugin startup failures observed 2026-10-20 with the pinned
%% registry (149b5bff): daemon stays active; plugins.manager only warns.
zara_plugin_startup_failure('zara-aidot', module_not_found('matter_server'),
    scope(pinned_registry_packaging_gap)).
zara_plugin_startup_failure('zara-discord', improper_token_passed,
    scope(secrets_env_token_invalid_or_absent)).
