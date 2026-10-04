% gpt-todos private Org workspace sync knowledge.
% Verified 2026-10-03 during gpt-todos-notes-import (dc0798ed).

% Ownership and deployment chain.
source_of_truth('scripts/gpt-todos-sync.org', 'scripts/gpt-todos-sync').
deploys('nix/home-manager/mods/gpt-todos.nix', 'gpt-todos-sync binary',
    'inlines scripts/gpt-todos-sync via builtins.readFile; installed binary only changes after home-manager switch').
deploys('nix/home-manager/files/global-agents.md', '~/.config/opencode/AGENTS.md',
    'via agent-verification.nix policyFile; edit the dotfiles source, never the deployed copy').
sync_target('~/Documents/Notes/org', '~/Documents/gpt-todos', 'git@github.com:lost-rob0t/gpt-todos.git (private, main)').

% Exclusions enforced by ignored_notes_rel.
sync_excludes(['.env', '.envrc', '.orgids', '.projectile', 'org-roam caches',
    'org-id-locations', '__pycache__', '.cache', '.git', '#lock#', '*~']).
invariant(credentials_out_of_repo,
    'credential files (.env/.envrc) and machine-local markers never enter the durable repo; a live API key was once staged by an interrupted run and caught only by manual inspection').

% Failure modes (both seen 2026-10-03).
failure_mode(dirty_checkout_refusal,
    'sync exits 6 while agenda/ or notes/ is dirty (staged or untracked); an interrupted run left 2383 files staged so every timer tick failed until the staging was manually committed').
failure_mode(nested_gitignore_add_failure,
    'a nested project .gitignore inside notes/ (e.g. fts-agent) makes git add of mirrored derived paths fail the whole sync; keep derived caches like __pycache__ in ignored_notes_rel').
failure_mode(external_dir_symlinks_skipped,
    'symlinks to directories outside the workspace (roam/hacking/* -> ~/Documents/hackmode) hash as absent and are silently skipped; intentional safety, not a bug').

% Reconciliation semantics.
reconciliation(local_deletion, 'ignored; final copy-back restores the repo copy locally').
reconciliation(remote_ahead, 'script rebases its own unpushed commits onto upstream and pushes; fails closed on conflict').
repair_recipe(interrupted_staging,
    'unstage junk basenames, rm their repo copies, manually commit the legit staged set, push, then rerun gpt-todos-sync').

% Infrastructure quirks.
infra_quirk(forgejo_push_broken,
    'pushing nsaspy/dotfiles to git.starintel.actor fails server-side (remote unpack failed: unable to create temporary object directory) while auth and disk are fine; github remote is the working fallback').
infra_quirk(committed_conflict_markers,
    'flake.nix carried committed ======= markers around the zara input pin making HEAD unbuildable; symptom is an hm switch error pointing at flake.nix').
