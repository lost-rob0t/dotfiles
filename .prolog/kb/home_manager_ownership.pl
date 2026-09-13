% Home Manager activation ownership facts.

% Root cause (verified 2026-09-12):
% `home-manager switch` aborted in checkLinkTargets because
% ~/.config/systemd/user/qwen3-tts.service existed as a foreign
% regular file (mode 0444, backend cpu) while the flake config
% (services.qwen3-tts.backend = "rocm") generates its own unit at
% the same path.
hm_link_conflict(qwen3_tts_unit, '/.config/systemd/user/qwen3-tts.service').
hm_configured_backend(rocm).
foreign_unit_backend(cpu).

% The qwen3-tts CLI `install-user` subcommand writes that unit path
% directly (scripts/qwen3-tts install_user_service in the
% lost-rob0t/Qwen3-TTS_server input), so it must never be run on a
% host where Home Manager owns services.qwen3-tts.
cli_writes_hm_path('qwen3-tts install-user', '/.config/systemd/user/qwen3-tts.service').

% Repair rule: back up and remove the foreign file, then switch.
% Home Manager replaces it with a symlink into the generation store.
repair(hm_link_conflict(Unit)) :-
    hm_link_conflict(Unit, _),
    format("backup + remove foreign unit, then home-manager switch~n").

% Invariant: every path Home Manager links must either be a symlink
% into a home-manager-files store path or absent before activation.
hm_owns_path('/.config/systemd/user/qwen3-tts.service').
hm_owns_path('/.config/systemd/user/zara-server.service').
