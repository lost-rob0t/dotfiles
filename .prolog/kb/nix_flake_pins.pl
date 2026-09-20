% External flake pin and fast Home Manager evaluation knowledge.
% Verified 2026-09-19 during agent/emacs-llm-overhaul-pr3 (org-vector PR).

% The local checkout /home/unseen/Documents/Projects/vectored-notes
% publishes on GitHub as lost-rob0t/org-vector (its Git remote and its
% own flake meta.homepage agree). getFlake by the local directory name
% 404s on the GitHub tarball endpoint.
flake_pin_identity('github:lost-rob0t/org-vector',
                   'Vectored Notes local checkout',
                   '7dc56eec47be06a2df5bfe7dfe4ceb17167372a4').

% builtins.getFlake pins in nix/home-manager/mods follow the llm.nix
% pattern: <name>Flake = builtins.getFlake "github:owner/repo/<revision>";
% <name>Package = <name>Flake.packages.${pkgs.stdenv.hostPlatform.system}.default;
% There is no reachability guard. Flake evaluation is lazy, so a host
% that never forces the package never fetches the pinned flake.
get_flake_pin_style(lazy_unforced, disabled_hosts_skip_fetch).

% Fast Home Manager check (evaluation only, no build), mirroring CI
% nix.yml: nix eval --raw '.#homeConfigurations."unseen@desktop".activationPackage.drvPath'
% (~40s warm; the flake host config is the second target). In a Git
% worktree, NEW files must be git-added before evaluation: flake source
% gathering only sees Git-tracked paths and fails with "not tracked by
% Git".
hm_fast_eval(nix_eval_drvpath, worktree_requires_git_add).
