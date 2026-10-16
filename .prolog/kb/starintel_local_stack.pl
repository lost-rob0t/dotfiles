% Verified local StarIntel persistence and ownership invariants.

starintel_local_stack_profile(unseen_at_flake).
starintel_local_stack_root('/home/unseen/.starintel').
starintel_local_stack_couchdb_user(admin).

% Runtime values are operator-owned files. Nix owns the writer, paths and
% service wiring, but it must neither embed nor generate these secrets.
starintel_credential_boundary(owner_private_files_under_starintel_root).
starintel_credential_mode('0600').
starintel_credential_value_forbidden_in(nix_store).
starintel_credential_value_forbidden_in(git).
starintel_credential_value_forbidden_in(command_arguments).
starintel_credential_value_forbidden_in(logs).

% The migrated Docker named volumes remain an offline rollback source. The
% active stack uses bind-mounted directories below the local stack root.
starintel_legacy_volume_retained(starintel_couchdb_data).
starintel_legacy_volume_retained(starintel_clouseau_index).
starintel_legacy_volume_retained(starintel_rabbitmq_data).
starintel_legacy_volume_retained(starintel_valkey_data).

starintel_active_storage(bind_mounts_under_starintel_root).
