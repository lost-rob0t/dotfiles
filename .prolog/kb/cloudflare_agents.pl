:- multifile owner/2.
:- multifile invariant/2.
owner(cloudflare_agent_setup, 'nix/home-manager/mods/cloudflare.nix').
owner(cloudflare_skills, 'flake.nix cloudflare-skills input and flake.lock').
invariant(cloudflare_oauth, credentials_remain_client_runtime_state).
invariant(cloudflare_mutable_codex, 'apply-codex-config.py preserves unrelated tables and comments').
