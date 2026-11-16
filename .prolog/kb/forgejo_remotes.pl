%% Forgejo remote topology and auth state for this repository.
%% RESOLVED 2026-11-16 (operator correction, run-a8d450e): the earlier
%% "auth wall" was an operator error — agents tested the WRONG SSH USER.
%% The working credential is user `forgejo`, not `git`:
%%   ssh -o BatchMode=yes forgejo@git.starintel.actor
%%     -> "Hi there, nsaspy! ... authenticated with the key named
%%         nsaspy@DexOs ... no shell access"
%% Push verified: origin forgejo@git.starintel.actor:nsaspy/dotfiles.git
%% accepted master a8d450e7..c1389871 on 2026-11-16.
%% RULE: never record an ssh/blocker claim without naming the exact
%% user@host and command evidence; the git@ user was never valid here.

forgejo_auth_recipe(verified, 'forgejo@git.starintel.actor',
    evidence('ssh -o BatchMode=yes forgejo@git.starintel.actor -> Hi nsaspy, key nsaspy@DexOs; push a8d450e7..c1389871 succeeded 2026-11-16')).
forgejo_service_state(up, date(2026, 11, 16),
    evidence('SSH handshake and auth both answer; shell access intentionally absent')).
forgejo_origin_remote_path('forgejo@git.starintel.actor:nsaspy/dotfiles.git',
    status(canonical_origin),
    evidence('git remote set-url origin forgejo@git.starintel.actor:nsaspy/dotfiles.git; push succeeded')).

%% Historical record retained: keys were rejected under user `git` only.
%% That was an authorization fact about the WRONG USER, not an auth wall.
forgejo_rejected_user_git([id_ed25519, id_rsa, gitea, the_night_watch, spearhead, beast, ansible],
    date(2026, 11, 15),
    evidence('ssh -i <key> -o IdentitiesOnly=yes -T git@git.starintel.actor -> Permission denied (publickey); wrong SSH user, not a key problem')).

%% Repo-path question resolved the same day: origin is nsaspy/dotfiles and
%% the push above landed there; scripts/dotfiles-sync REMOTE default should
%% be confirmed against this origin, not treated as a competing authority.
forgejo_path_resolution(resolved_nsaspy_dotfiles, date(2026, 11, 16),
    'origin = forgejo@git.starintel.actor:nsaspy/dotfiles.git; syncer REMOTE default pending alignment with origin').

%% Disambiguation vs the historical quirk in gpt_todos_sync.pl:
%% forgejo_push_broken describes a server-side unpack failure (an
%% UNPACK-phase error implying successful auth) from an earlier incident.
quirk_distinction(forgejo_push_broken, unpack_phase, 'auth succeeded, server-side unpack failed (historical)').

%% GitHub mirror remains valid for outage fallback.
github_remote_auth(ok, date(2026, 11, 15),
    evidence('ssh -T git@github.com -> Hi lost-rob0t; push f851e313..619e8ddc succeeded')).
