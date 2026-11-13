% AgentProlog client setup, mirroring the opencode/codex client pattern.
% Source of truth: flake input `agentprolog` + nix/home-manager/mods/agentprolog.nix.

owner(agentprolog, 'git.starintel.actor/nsaspy/agentProlog').
owner(agentprolog_mirror, 'github.com/lost-rob0t/agentProlog').

deployment_path(agentprolog,
    'flake input agentprolog -> nix/home-manager/mods/agentprolog.nix -> llm.enable -> home.packages').

invariant(agentprolog_no_ambient_trust,
    'No global config is auto-loaded; ~/.config/agentprolog/operator.pl is inert until passed via --config').
invariant(agentprolog_state_identity,
    'Durable state binds project+config identity; use one --state dir per agent profile').
invariant(agentprolog_runtime_pinned,
    'prolog-rlm runtime rev is pinned inside the agentprolog input lock; never point sessions at a moving runtime').
invariant(agentprolog_main_secret_offline,
    'agentprolog-main key (unlimited) lives only in ~/.config/openrouter/agentprolog-main-key (0600); consumed via OPENROUTER_API_KEY by the agentprolog-main wrapper at runtime, never via Nix values or Git').

credential(agentprolog_main, unlimited_openrouter_key).
credential_slot(agentprolog_main, '~/.config/openrouter/agentprolog-main-key -> OPENROUTER_API_KEY (agentprolog-main wrapper)').
deployment_path(agentprolog_main_wrapper,
    'mods/agentprolog.nix mainOperatorConfig/mainKeyFile -> xdg.configFile agentprolog/main.pl + home.packages agentprolog-main').

infra_quirk(agentprolog_input_fetch,
    'Flake input uses git+https on canonical Forgejo because the GitHub remote is only a mirror').

failure_mode(agentprolog_config_binding_mismatch,
    'Reused --state dir after changing operator config/agent: start a new state dir or migrate explicitly (upstream issue #61)').
failure_mode(agentprolog_model_turn_without_provider,
    'session.turn fails closed without an explicitly configured provider; /report-bug needs a model-capable session').

repair_recipe(agentprolog_stale_input,
    'nix flake update agentprolog, review the new agentProlog rev, rebuild home config').

% Verified against agentProlog head df67f42fdb (branch feat/prolog-config-crud-e2e,
% pending merge as PR #55) and prolog-rlm 4716ba8aa0bbf3fa1b96d80e8ed8e91d99973c85.
