% profile_wipe_incident.pl -- 2026-11-17 home-manager profile wipe + GPU
% artifacting incident on host flake (unseen@flake). Facts verified during
% the live repair; attribution where provable, unknown where not.

%% SYMPTOMS (operator-reported + observed):
%% - home-manager, distro, all HM packages missing from PATH
%% - user profile generation 325 (2026-11-17 03:29) was the ONLY generation
%%   and its target directory was EMPTY (bin had 0 entries)
%% - ~/.config/systemd/user/*.service were dangling symlinks into
%%   garbage-collected store paths -> zara units exec-failed and
%%   restart-looped (zara-pair failed, zara-wake auto-restart, python3
%%   spinner at 98.5% CPU retrying)
%% - GPU artifacting: a detached benchmark job from ~23h earlier was still
%%   alive: /bin/bash -c running llama-tts with pocket-tts GGUFs on card1
%%   plus a VRAM poller looping `cat mem_info_vram_used` every 0.25s
%% - emacs shell saw constant datetime/ffprobe/sysfs churn from that loop

incident_symptom(empty_profile_generation, date(2026, 11, 17),
    'gen 325 sole survivor, target dir empty; all HM binaries gone from PATH').
incident_symptom(dangling_user_units, date(2026, 11, 17),
    'systemd user units pointed into GCed store paths; zara restart-looped').
incident_symptom(stale_benchmark_job, date(2026, 11, 17),
    'pid 1225269: 23h-old llama-tts VRAM benchmark + 0.25s sysfs poller; killed with its process tree').
incident_symptom(operator_saw_datetime_storm, date(2026, 11, 17),
    'the 0.25s poller + crash-loop churn read as "AI typing 40 datetime queries per minute" in emacs shell').

%% CAUSE: the user profile was wiped from OUTSIDE home-manager. No
%% nix-gc.timer exists on this Arch host (systemctl list-timers: shadow,
%% systemd-tmpfiles-clean, archlinux-keyring only), and no GC process was
%% caught live -- so the wipe was an explicit `nix-collect-garbage` /
%% `nix profile` / wipe-history style invocation by an agent session
%% (bursts between 2026-11-16 21:23 and 2026-11-17 03:29). Attribution
%% unknown; do not guess in future records, and ASK before running any
%% profile/garbage operation on a machine home-manager manages.
incident_cause(profile_wiped_outside_hm, date(2026, 11, 17),
    'HM user profile replaced with an empty generation and history deleted; no nix-gc.timer armed -> manual/agent-invoked').

%% REPAIR (all verified live):
%% 1. nix build .#homeConfigurations."unseen@flake".activationPackage
%%    (flake dirty zara pin cd6bde72 kept: that build was already verified
%%    by the earlier agent; KB note in zara_deployment.pl)
%% 2. activate from the generation outPath: <gen>/activate
%% 3. HM clobber guard: ~/.config/opencode/cli.json was being recreated
%%    every second by live opencode sessions, beating every mv+activate
%%    race. FIX: pre-seed the path as a symlink to the generation's own
%%    managed copy
%%    (ln -sfn <gen>/home-files/.config/opencode/cli.json ...) so the
%%    clobber check passes, then activate immediately.
%% 4. kill stale process trees (benchmark bash; old opencode-2.2.5 serve)
%% 5. zara TTS off the GPU: hand-managed ~/.config/zarathushtra/config.toml
%%    provider llamacpp -> edge. GOTCHA: "none" is NOT a valid provider;
%%    choices are 11labs, edge, llamacpp, local, qwen3; "none" crash-loops
%%    zara-server with ConfigError every ~6s.
%% 6. restart zara-server AFTER the config edit lands (parallel edit+restart
%%    races: the unit reads config at exec).
incident_repair(activate_recipe, date(2026, 11, 17),
    'build activationPackage, pre-seed raced symlinks to <gen>/home-files/..., run <gen>/activate, then daemon-reload').
incident_repair(tts_gpu_off, date(2026, 11, 17),
    'config.toml provider=edge; zero llama-tts processes; zara-server active, zero ConfigErrors').

%% RULES for agents on this machine:
incident_rule(never_touch_user_profile,
    'NEVER run nix-collect-garbage -d, wipe-history, nix-env -e "*", or nix profile install/remove on this host without asking the operator: home-manager owns the user profile').
incident_rule(dangling_units_mean_profile_loss,
    'systemd --user units failing with missing store paths means the HM profile was wiped; repair by rebuild+activate, never by hand-editing unit files').
incident_rule(benchmark_jobs_must_die_with_their_session,
    'long-running GPU benchmark loops detached from agent shells outlive the agent and wreck the GPU; clean up child processes before ending a session').
incident_rule(cli_json_race_recipe,
    'opencode sessions rewrite ~/.config/opencode/cli.json continuously; to activate HM over it, pre-seed HM symlink then activate in the same shell').
incident_rule(tts_provider_choices,
    'zara TTS providers: 11labs, edge, llamacpp, local, qwen3; edge = remote, zero GPU; llamacpp/local = local GPU').
