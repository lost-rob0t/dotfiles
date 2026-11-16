%% llm-log deploy and capture-plane knowledge (verified 2026-11-02, task llmlog-zara-tts).

%% Upstream authority: github only; the Forgejo mirror does not exist.
llm_log_pin_source('github:lost-rob0t/llm-log', note('git.starintel.actor/lost-rob0t/llm-log returns not found; the github pin is canonical')).

%% Module compatibility 692d3820 -> 0679212a (23 commits): purely additive.
llm_log_module_compat(0679212a,
    unchanged([enable, package, dataDir, expert, upstreams, extraArgs]),
    added([quotas_enable, quotas_package, quotas_port, quotas_refresh_seconds,
           quotas_cache_file, quotas_zai_key_file, quotas_environment_file,
           quotas_codex_home]),
    note('services.llm-log.quotas.* independent poller defaults to disabled')).
llm_log_revision_features(0679212a,
    [provider_reported_subscription_quota_telemetry, subscription_history_capture_export,
     internal_worker_attribution_stripping]).

%% Auth model: the proxy is a transparent forwarder.
llm_log_proxy_auth(forward, client_authorization_header).
llm_log_proxy_auth(persist, redacted_copies_only).
llm_log_upstream_path(zai, '/zai/api/coding/paas/v4').

%% Quota telemetry credentials (quotas.py reads Z_AI_API_KEY or ZAI_API_KEY).
llm_log_zai_credential(source('opencode auth.json provider zai-coding-plan'),
    runtime_file('~/.config/llm-log/quotas.env'), var('ZAI_API_KEY'), mode(0600)).
llm_log_zai_quota_failure(credentials_unavailable,
    cause('missing Z_AI_API_KEY/ZAI_API_KEY on the llm-log unit (EnvironmentFile)')).

%% Codex consumer: same key, same capture plane.
codex_zai_provider(option('codex.llmLog.zaiCodingPlan'),
    profile(zai), model_default('glm-5.3'), env_key('ZAI_API_KEY'),
    wrapper_sources('~/.config/llm-log/quotas.env')).
