zara_deployment_owner(home_manager, 'unseen@flake').
zara_deployment_input(zara, 'flake.lock').
zara_deployment_services(['zara-server.service', 'zara-pair.service', 'zara-wake.service', 'zara-desktop.service']).
zara_deployment_dependency(qwen3_tts_service, zara_tts_provider, 'http://127.0.0.1:7860').
zara_runtime_state(enabled, date('2026-11-02'), operator_request).
qwen3_tts_state(disabled, date('2026-11-02'),
    operator_decision('tts container implicated in the crash investigation; zara runs without voice')).
zara_deployment_invariant(preserve_other_dirty_lock_inputs).
zara_deployment_invariant(compare_service_execstart_to_evaluated_package).
zara_startup_observation(wake_may_retry_before_daemon_ready).
zara_startup_observation(pair_may_fail_before_control_socket_exists).

%% Deployed 2026-11-17 on flake host (verified, gen165, zarathushtra-full k1hmv0w5):
%% flake input zara pin bumped 034d6b23 -> cd6bde72 (314 commits); other lock
%% inputs preserved; [tts] provider llamacpp in hand-managed config.toml
%% (engine defaults cover binary/model/mmproj/ngl; pocket-tts GGUFs on disk).
zara_deployment_trap(systemd_dropin_execstart_override,
    '~/.config/systemd/user/<unit>.service.d/override.conf silently pins a stale
     binary over every HM generation (server+wake pinned 2026-09-27 old pin);
     KB invariant compare_service_execstart_to_evaluated_package cannot pass
     until such drop-ins are removed; archive to /tmp before rm -r the .d dir').
zara_deployment_state(active, date(2026, 11, 17),
    [zara-server, zara-pair, zara-wake, zara-desktop],
    execstart(k1hmv0w5zarathushtra_full, remote_tcp_6060_securitydir)).
zara_server_degraded_scope(llm_credentials_only,
    'OPENROUTER_API_KEY not wired into server unit env; voice+Prolog unaffected').

%% Enabled-state check contract (verified 2026-11-02 re-enable on flake):
%% checks.zara-server and tests/zara-server.nix assert the flake profile
%% ExecStart carries --remote-endpoint tcp://0.0.0.0:6060 + --security-dir,
%% never a primary ' --endpoint ', and the shared desktop profile binds no
%% remote endpoint. checks.zara-all-plugins asserts every pinned registry
%% plugin lands in home.packages plus its discovery and <name>/lib files.
%% tests/zara-workflows.sh greps desktop home.nix for workflows.enable=true,
%% plugins enableAll=true, and qwen3-tts enable/autoStart=false markers
%% (TTS stays disabled while Zara runs).
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

%% Upstream issues filed 2026-11-03 from the flake deploy session (zara @ 034d6b23).
zara_upstream_issue(1527, startup_degraded_warning, 'optional OpenAI credentials absent marks whole runtime degraded').
zara_upstream_issue(1528, kb_intents_discontiguous, 'python_skill_intent/3 needs :- discontiguous (intents.pl:121 vs :160)').
zara_upstream_issue(1492, voice_ingress_queue_full, 'evidence comment added: 13 terminal failures/90min with tts endpoint disabled').
