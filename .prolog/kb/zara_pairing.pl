% Zara localhost pairing and platform keyring selection knowledge.
% Promoted from the zara-pair task (see .prolog/runs/ for raw evidence).

%% Architecture
zara_pair_tool('zara-pair', owner(zara_workflows_module)).
zara_pair_expert('.zara/experts/keyring', owns(platform_keyring_enumeration)).
zara_pair_keyring_schema(service('zara-daemon'), account(desktop_host), fields([curve_public_key, curve_secret_key])).
zara_pair_env_file('~/.config/zarathushtra/secrets-daemon-clients.env', mode('0600'), generated_by('zara-pair')).

%% Live security admin control plane
zara_control_socket('$XDG_RUNTIME_DIR/zarathushtra/zara-control.sock').
zara_admin_protocol(json_newline, one_request_per_connection, socket_mode('0600')).
zara_admin_action(enroll, fields([public_key, device_id])).
zara_admin_action(revoke, fields([device_id])).
zara_admin_action(list, fields([])).
zara_admin_action('remote_listener.ensure', returns([active, endpoint, server_public_key])).
zara_admin_action('remote_listener.status', returns([active, endpoint, server_public_key])).

%% Invariants
zara_pair_invariant(keyring_is_source_of_truth).
zara_pair_invariant(partial_keypair_fails_closed).
zara_pair_invariant(no_plaintext_keyring_fallback).
zara_pair_invariant(env_file_quoted_for_systemd_and_bash).
zara_pair_invariant(revoking_other_devices_is_manual).

%% Discovered failure modes
zara_pair_failure(pyzmq_curve_keypair_returns_bytes, fix(decode_ascii)).
zara_pair_failure(secret_tool_attrs_are_alternating_pairs, fix(key_value_argv_elements)).
zara_pair_failure(z85_values_break_bash_sourcing, fix(single_quote_env_values)).
zara_pair_failure(daemon_restart_rotates_advertised_endpoint, fix(rerun_pair_before_client_start)).

%% Runtime facts observed on flake
zara_pair_runtime(backend(freedesktop_secrets), provider(gnome_keyring)).
zara_pair_runtime(account('desktop-flake'), verified(turn_accepted_at('tcp://10.50.50.28:17865'))).
