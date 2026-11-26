% pi (pi-coding-agent) extension contract facts, learned while reviving
% PR #341 (pi rlm-ops) through merge-on-green on 2026-11-25.

% pi loads extensions from settings, not CLI flags: there is no
% --extension <dir> load flag on pi 1.4.x or 1.6.x. In-session commands
% registered via pi.registerCommand (e.g. /symbolic) are not CLI flags.
pi_extension_load_contract('pi install <source> [-l] into settings').

% pi 1.6 installs absolute-path sources but records them in a way
% `pi list` does not report; relative in-project paths register on both
% 1.4.0 and 1.6.1. Install smoke tests must copy the extension into the
% target project and use a relative path, project-locally (-l --approve)
% inside a throwaway dir so the operator's own pi settings stay untouched.
pi_extension_install_smoke(relative_path_required, 'pi 1.6 list misses absolute-path installs').

% bg process management portability rule (pi rlm-ops bg.ts): track task
% state on the child "exit" event, never "close". close waits for the
% stdio pipes; /bin/sh -c single commands are exec'd by bash but FORKED
% by dash (Debian containers), so a killed shell's orphan child holds
% the pipes open and stalls every close-driven kill/timeout transition
% until the orphan exits naturally. Reproduced in a node:24-bookworm
% container (dash forks) vs NixOS host (bash execs).
bg_state_signal(exit_not_close, 'dash forks -c commands; bash execs them').
