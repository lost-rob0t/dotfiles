:- module(dotfiles_bluetooth_audio_expert,
    [ fact/2, observe/2, reset_facts/0, hypothesis/1, next_probe/2,
      verification/2 ]).

/** <module> dotfiles_bluetooth_audio_expert

Deterministic (model-free) diagnosis rules for Bluetooth audio on flake.
Encodes the 2026-11 Arctis Nova 7 / Realtek BT 5.3 incident root causes:

- stale in-memory A2DP SEP state in long-running bluetoothd after PipeWire
  endpoint churn ("load_remote_sep() Unable to load LastUsed: lseid/rseid
  not found", "a2dp-sink profile connect failed: Device or resource busy");
- Realtek BT radio (0bda:a729) USB autosuspend losing HCI completion
  reports mid-stream and killing PipeWire transports;
- WirePlumber autoswitch-to-headset-profile flipping to mSBC/CVSD;
- drift of the persistent fixes (udev rule, modprobe conf, WP drop-in).

Observed fact vocabulary (K, V):

| codec                    | sbc_xq|sbc|aac|aptx|ldac|msbc|cvsd|none  |
| active_profile           | a2dp_sink_sbc_xq|a2dp_sink|headset_*|off|none |
| a2dp_profile_available   | yes|no                                  |
| stale_sep_errors         | integer >= 0 (journal since current bluetoothd start) |
| a2dp_connect_failures    | integer >= 0 (journal since current bluetoothd start) |
| transport_failures       | integer >= 0 (journal since current WP start)         |
| completion_report_errors | integer >= 0 (journal since current WP start)         |
| usb_autosuspend          | enabled|disabled|unknown (sysfs power/control)  |
| udev_rule_present        | yes|no                                  |
| modprobe_conf_present    | yes|no                                  |
| wp_dropin_present        | yes|no                                  |
| transport_failures_recent    | integer >= 0 (last 15 min)          |
| completion_report_errors_recent | integer >= 0 (last 15 min)      |
*/

:- dynamic fact/2.
:- discontiguous hypothesis/1.
:- discontiguous next_probe/2.
:- discontiguous verification/2.

observe(Key, Value) :-
    retractall(fact(Key, _)),
    assertz(fact(Key, Value)).

reset_facts :- retractall(fact(_, _)).

% helpers ---------------------------------------------------------------

count_gt(Key) :-
    fact(Key, N),
    integer(N),
    N > 0.

degraded_codec(Code) :-
    fact(codec, Code),
    memberchk(Code, [msbc, cvsd]).

a2dp_profile_active :-
    fact(active_profile, P),
    memberchk(P, [a2dp_sink, a2dp_sink_sbc_xq]).

% H1: stale bluetoothd SEP cache ----------------------------------------
% bluetoothd holds A2DP endpoint registrations from a dead PipeWire
% instance; A2DP connects fail while the device is connected.
% Blocking signals: explicit profile-connect failures, or missing A2DP
% profiles together with SEP-cache mismatch evidence.
hypothesis(stale_bluetoothd_sep_cache) :-
    count_gt(a2dp_connect_failures).
hypothesis(stale_bluetoothd_sep_cache) :-
    fact(a2dp_profile_available, no),
    count_gt(stale_sep_errors).
next_probe(stale_bluetoothd_sep_cache, restart_bluetooth_then_reconnect).
verification(stale_bluetoothd_sep_cache, a2dp_sbc_xq_active_after_reconnect).

% H2: HFP fallback while A2DP is negotiable -----------------------------
% The card offers a2dp-sink but sits on a headset profile (mSBC/CVSD).
hypothesis(hfp_fallback_profile) :-
    degraded_codec(_),
    fact(a2dp_profile_available, yes),
    \+ a2dp_profile_active.
next_probe(hfp_fallback_profile, select_a2dp_sbc_xq_profile).
verification(hfp_fallback_profile, codec_is_sbc_xq_or_aac).

% H3: Realtek autosuspend transport flap --------------------------------
% Adapter wakes mid-stream and loses HCI completion reports; transports
% error out until reconnect. Recent-window counts gate the flap; the
% hazard state itself (autosuspend re-enabled on the radio) also fires.
hypothesis(realtek_autosuspend_flap) :-
    count_gt(transport_failures_recent).
hypothesis(realtek_autosuspend_flap) :-
    count_gt(completion_report_errors_recent).
hypothesis(realtek_autosuspend_flap) :-
    fact(usb_autosuspend, enabled).
next_probe(realtek_autosuspend_flap,
                verify_autosuspend_disabled_and_rules_present).
verification(realtek_autosuspend_flap, no_transport_failures_after_next_connect).

% H4: persistent-fix drift ----------------------------------------------
hypothesis(config_drift) :- fact(udev_rule_present, no).
hypothesis(config_drift) :- fact(modprobe_conf_present, no).
hypothesis(config_drift) :- fact(wp_dropin_present, no).
next_probe(config_drift, restore_persistent_fixes).
verification(config_drift, all_persistent_fix_files_present).

% H5: healthy -----------------------------------------------------------
hypothesis(healthy) :-
    a2dp_profile_active,
    \+ count_gt(transport_failures_recent),
    \+ count_gt(completion_report_errors_recent),
    \+ count_gt(stale_sep_errors),
    \+ count_gt(a2dp_connect_failures),
    fact(usb_autosuspend, disabled),
    fact(udev_rule_present, yes),
    fact(modprobe_conf_present, yes),
    fact(wp_dropin_present, yes).
next_probe(healthy, none).
verification(healthy, periodic_probe_on_reconnect).
