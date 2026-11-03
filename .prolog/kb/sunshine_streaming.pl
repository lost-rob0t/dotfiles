% Sunshine game streaming host (games.nix gaming.sunshine) — verified facts.
% All facts verified 2026-11-03 against sunshine 2026.914.233613 (nixpkgs)
% on host flake (Arch, qtile/X11, AMD RX 5500 RDNA1, PipeWire, 3x1080p).

% Config option surface of this sunshine build (from smoke-test logs):
sunshine_unknown_option(codec).       % "Unrecognized configurable option [codec]"
sunshine_unknown_option(adapter).     % codec negotiation and adapter selection
                                      % are client/startup driven instead
sunshine_valid_option(output_name).   % works for capture = x11, not just kms

% KMS capture hard-exits without CAP_SYS_ADMIN ("Fatal: Unable to find
% display or encoder") — it does NOT fall back to another capture method.
sunshine_kms_requires_cap_sys_admin.
% A setcap capability on the /nix/store sunshine binary is lost whenever
% nix rebuilds the package, and home-manager-updater would then leave the
% unit crash-looping on a pinned kms capture.
sunshine_kms_setcap_not_update_safe.

% capture = x11 with output_name set streams ONE display; with output_name
% unset it warns "defaulting to recording entire virtual desktop" and
% streams the stitched 5760x1080 desktop.
sunshine_x11_capture_selects_display(output_name).

% The RDNA1 encode block does H.264 and HEVC via VAAPI only; the AV1
% probe fails ("No usable encoding profile found") and is benign noise,
% like the CAP_SYS_ADMIN probe lines.
sunshine_rdna1_encoders([h264, hevc]).
sunshine_benign_log_noise(['Failed to gain CAP_SYS_ADMIN', 'av1_vaapi', 'Daemon not running']).

% Home Manager owns ~/.config/sunshine/{sunshine.conf,apps.json}; Sunshine's
% sample files must be removed once before the first switch or activation
% aborts with a clobber warning (see home_manager_ownership.pl repair rule).
hm_owns_path('/.config/sunshine/sunshine.conf').
hm_owns_path('/.config/sunshine/apps.json').

% Deployment history: generation 99 on host flake was built from the dirty
% main checkout (newer skills pin than clean master).  Never activate from a
% clean worktree on this host while the live generation tracks uncommitted
% work — land the changed module file into the main checkout and switch from
% there instead.
live_generation_built_from(dirty_main_checkout, 99).
deploy_rule(flake, land_module_in_main_checkout_then_switch).
deploy_exception(flake, home_manager_updater_masked).  % branch merge does not deploy
