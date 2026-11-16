%% Forgejo remote topology and auth state for this repository.
%% All facts verified 2026-11-15 from a non-interactive agent shell.
%% Correction record: an earlier claim framed git.starintel.actor as
%% broken/outage — RETRACTED. The SSH service is up; the wall is client
%% auth (no accepted key from this shell) plus a repo-path mismatch.

%% Service state: the server completes the SSH handshake and answers in
%% the auth phase (publickey rejection), for both repo paths. A refused
%% key is an authorization answer, not a service outage.
forgejo_service_state(up, date(2026, 11, 15),
    evidence('ssh -T git@git.starintel.actor -> Permission denied (publickey); handshake completes, auth-phase answer')).

%% Canonical repo path per scripts/dotfiles-sync (REMOTE default):
%% lost-rob0t/dotfiles. The checkout's origin remote points at
%% nsaspy/dotfiles — a different path on the same host. This mismatch is
%% unresolved: which path is authoritative needs operator confirmation.
forgejo_canonical_repo('git@git.starintel.actor:lost-rob0t/dotfiles.git',
    evidence('scripts/dotfiles-sync line 5 REMOTE default')).
forgejo_origin_remote_path('git@git.starintel.actor:nsaspy/dotfiles.git',
    status(mismatch_with_canonical),
    evidence('git remote -v')).
forgejo_path_resolution(open, date(2026, 11, 15),
    'operator must confirm canonical path: lost-rob0t/dotfiles (syncer default) vs nsaspy/dotfiles (origin remote)').

%% Auth wall: every unencrypted private key in ~/.ssh is rejected by
%% git@git.starintel.actor. Not yet tested: pi_rsa (passphrased), agent
%% forwarded keys, HTTPS/token auth, keys registered under a different
%% account. Unknown != impossible: the correct credential may simply not
%% be reachable from this shell.
forgejo_rejected_keys([id_ed25519, id_rsa, gitea, the_night_watch, spearhead, beast, ansible],
    date(2026, 11, 15),
    evidence('ssh -i <key> -o IdentitiesOnly=yes -o BatchMode=yes -T git@git.starintel.actor -> Permission denied (publickey) for each')).
forgejo_untested_auth([pi_rsa_passphrased, https_token, tea_api, operator_session_agent],
    reason('no tty for passphrase entry; no agent identity visible from agent shells')).

%% Agent-shell environment facts: agent shells run with empty
%% SSH_AUTH_SOCK; the gpg-agent SSH socket exists but holds no identities.
agent_shell_fact(ssh_auth_sock_empty, date(2026, 11, 15),
    'non-interactive shells inherit no SSH_AUTH_SOCK; test ssh auth with SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh first').
agent_shell_fact(gpg_agent_no_identities, date(2026, 11, 15),
    evidence('ssh-add -l via /run/user/1000/gnupg/S.gpg-agent.ssh -> The agent has no identities')).

%% Disambiguation vs the historical quirk in gpt_todos_sync.pl:
%% forgejo_push_broken describes a server-side unpack failure (an
%% UNPACK-phase error implying successful auth) from an earlier incident.
%% The 2026-11-15 wall is an AUTH-phase rejection. Different failure
%% modes; do not conflate them again.
quirk_distinction(forgejo_push_broken, unpack_phase, 'auth succeeded, server-side unpack failed (historical)').
quirk_distinction(forgejo_auth_wall_2026_11_15, auth_phase, 'keys rejected before any repo access; service up').

%% Working fallback while the Forgejo credential is unresolved: GitHub
%% (lost-rob0t/dotfiles) authenticated and accepted the rescue push
%% (f851e313..619e8ddc, 2026-11-15).
github_remote_auth(ok, date(2026, 11, 15),
    evidence('ssh -T git@github.com -> Hi lost-rob0t; push f851e313..619e8ddc succeeded')).
