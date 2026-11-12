% Deployment implementation remains in the starintel-conky repository.
starintel_conky_owner(runtime, 'nsaspy/starintel-conky').
starintel_conky_owner(deployment, 'scripts/starintel-conky.org').
% Literate pairs registered per the editing contract (AGENTS.md loader).
literate_pair('scripts/starintel-conky.org', '.config/starintel-conky-release.json').
literate_pair('scripts/starintel-conky.org', '.local/bin/starintel-conky').
literate_pair('scripts/starintel-conky.org', 'scripts/install-starintel-conky').
literate_pair('scripts/starintel-conky.org', 'scripts/install-starintel-mq-python').
starintel_conky_pin('.config/starintel-conky-release.json').
starintel_conky_runtime('XDG_DATA_HOME/starintel-conky/current').
starintel_conky_private_config('XDG_CONFIG_HOME/starintel-conky/config').
starintel_conky_startup('.config/qtile/scripts/autostart.sh', detached_idempotent_start).
starintel_conky_invariant(single_configurable_monitor, below_applications).
starintel_conky_invariant(queue_monitoring, read_only_no_dequeue_or_requeue).
starintel_conky_feed_default(public_sse_metadata).
starintel_conky_feed_private(rabbitmq_exclusive_subscription, new_and_updated_document_copies_only).
starintel_conky_feed_draw(lua_cache, interval_seconds(0.1)).
starintel_conky_optional_mq_python('scripts/install-starintel-mq-python', pinned_nixpkgs_gc_root).
