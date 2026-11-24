:- use_module(library(plunit)).
:- use_module('../kb/expert').

:- begin_tests(dotfiles_bluetooth_audio_expert, [setup(reset_facts), cleanup(reset_facts)]).

% H1: stale bluetoothd SEP cache (2026-11-23 recurrence signature)
test(stale_sep_cache_connect_failures) :-
    reset_facts,
    observe(a2dp_connect_failures, 7),
    observe(stale_sep_errors, 3),
    observe(a2dp_profile_available, no),
    observe(codec, cvsd),
    observe(active_profile, headset_head_unit),
    hypothesis(stale_bluetoothd_sep_cache),
    next_diagnostic(stale_bluetoothd_sep_cache, restart_bluetooth_then_reconnect),
    verification(stale_bluetoothd_sep_cache, a2dp_sbc_xq_active_after_reconnect).

test(stale_sep_cache_missing_profiles_with_evidence) :-
    reset_facts,
    observe(a2dp_connect_failures, 0),
    observe(stale_sep_errors, 4),
    observe(a2dp_profile_available, no),
    hypothesis(stale_bluetoothd_sep_cache).

% H1 negative: SEP-cache noise during clean renegotiation must not diagnose
test(no_stale_diagnosis_from_benign_cache_noise, [fail]) :-
    reset_facts,
    observe(a2dp_connect_failures, 0),
    observe(stale_sep_errors, 1),
    observe(a2dp_profile_available, yes),
    hypothesis(stale_bluetoothd_sep_cache).

% H2: HFP fallback with A2DP negotiable
test(hfp_fallback_switches_profile) :-
    reset_facts,
    observe(codec, msbc),
    observe(active_profile, headset_head_unit),
    observe(a2dp_profile_available, yes),
    hypothesis(hfp_fallback_profile),
    next_diagnostic(hfp_fallback_profile, select_a2dp_sbc_xq_profile),
    verification(hfp_fallback_profile, codec_is_sbc_xq_or_aac).

test(no_hfp_fallback_when_a2dp_active, [fail]) :-
    reset_facts,
    observe(codec, sbc_xq),
    observe(active_profile, a2dp_sink_sbc_xq),
    observe(a2dp_profile_available, yes),
    hypothesis(hfp_fallback_profile).

% H3: Realtek autosuspend flap
test(realtek_flap_from_transport_failures) :-
    reset_facts,
    observe(transport_failures_recent, 2),
    observe(completion_report_errors_recent, 0),
    hypothesis(realtek_autosuspend_flap),
    next_diagnostic(realtek_autosuspend_flap, verify_autosuspend_disabled_and_rules_present),
    verification(realtek_autosuspend_flap, no_transport_failures_after_next_connect).

test(realtek_flap_from_completion_reports) :-
    reset_facts,
    observe(transport_failures_recent, 0),
    observe(completion_report_errors_recent, 5),
    hypothesis(realtek_autosuspend_flap).

test(realtek_flap_from_rearmed_autosuspend) :-
    reset_facts,
    observe(transport_failures_recent, 0),
    observe(completion_report_errors_recent, 0),
    observe(usb_autosuspend, enabled),
    hypothesis(realtek_autosuspend_flap).

test(no_flap_when_disabled_and_quiet, [fail]) :-
    reset_facts,
    observe(transport_failures_recent, 0),
    observe(completion_report_errors_recent, 0),
    observe(usb_autosuspend, disabled),
    hypothesis(realtek_autosuspend_flap).

% H4: persistent-fix drift
test(drift_missing_udev_rule) :-
    reset_facts,
    observe(udev_rule_present, no),
    hypothesis(config_drift),
    next_diagnostic(config_drift, restore_persistent_fixes),
    verification(config_drift, all_persistent_fix_files_present).

test(drift_missing_modprobe_conf) :-
    reset_facts,
    observe(modprobe_conf_present, no),
    hypothesis(config_drift).

test(drift_missing_wp_dropin) :-
    reset_facts,
    observe(wp_dropin_present, no),
    hypothesis(config_drift).

% H5: healthy chain
test(healthy_state) :-
    reset_facts,
    observe(codec, sbc_xq),
    observe(active_profile, a2dp_sink_sbc_xq),
    observe(a2dp_profile_available, yes),
    observe(transport_failures_recent, 0),
    observe(completion_report_errors_recent, 0),
    observe(usb_autosuspend, disabled),
    observe(udev_rule_present, yes),
    observe(modprobe_conf_present, yes),
    observe(wp_dropin_present, yes),
    hypothesis(healthy),
    next_diagnostic(healthy, none),
    verification(healthy, periodic_probe_on_reconnect).

test(no_healthy_while_flapping, [fail]) :-
    reset_facts,
    observe(codec, sbc_xq),
    observe(active_profile, a2dp_sink_sbc_xq),
    observe(a2dp_profile_available, yes),
    observe(transport_failures_recent, 3),
    observe(completion_report_errors_recent, 0),
    observe(stale_sep_errors, 0),
    observe(a2dp_connect_failures, 0),
    observe(usb_autosuspend, disabled),
    observe(udev_rule_present, yes),
    observe(modprobe_conf_present, yes),
    observe(wp_dropin_present, yes),
    hypothesis(healthy).

test(no_healthy_with_drift, [fail]) :-
    reset_facts,
    observe(codec, sbc_xq),
    observe(active_profile, a2dp_sink_sbc_xq),
    observe(a2dp_profile_available, yes),
    observe(stale_sep_errors, 0),
    observe(a2dp_connect_failures, 0),
    observe(udev_rule_present, yes),
    observe(modprobe_conf_present, yes),
    observe(wp_dropin_present, no),
    hypothesis(healthy).

test(no_healthy_when_probe_evidence_missing, [fail]) :-
    reset_facts,
    observe(codec, sbc_xq),
    observe(active_profile, a2dp_sink_sbc_xq),
    observe(a2dp_profile_available, yes),
    observe(stale_sep_errors, 0),
    observe(a2dp_connect_failures, 0),
    observe(usb_autosuspend, disabled),
    observe(udev_rule_present, yes),
    observe(modprobe_conf_present, yes),
    observe(wp_dropin_present, yes),
    % fresh-flap facts absent: simulates a probe that crashed mid-run
    hypothesis(healthy).

:- end_tests(dotfiles_bluetooth_audio_expert).

:- initialization(main, main).
main :- (run_tests -> halt(0) ; halt(1)).
