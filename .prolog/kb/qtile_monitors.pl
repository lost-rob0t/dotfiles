% Verified 2026-10-26 against live XRandR EDIDs and renamed-output tests.
qtile_monitor_profile(flake, '.config/qtile/scripts/monitors/unseen@flake.sh').
qtile_monitor_identity(flake, edid_sha256).
qtile_monitor_name_limitation(displayport, enumeration_can_change_between_boots).
qtile_monitor_missing_policy(flake, preserve_existing_layout).
