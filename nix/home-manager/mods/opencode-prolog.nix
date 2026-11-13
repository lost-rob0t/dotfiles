# Declarative scaffolding for the agent-Prolog lane: a `prolog-bootstrap` CLI
# that seeds a target directory with the cwd-independent verifier state, the
# strict journal gate from the pinned skills input, and inert handoff/floors
# templates. The heavy lifting (cwd-independent init, bounded digests, strict
# gate) lives in the skills repo; this module only wires and stages it.
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.agentProlog.bootstrap;
  # keep the templates out of the script body so shellcheck never sees Prolog
  floorsFile = pkgs.writeText "prolog-bootstrap-floors.pl" floorsTemplate;
  handoffFile = pkgs.writeText "prolog-bootstrap-handoff.pl" handoffTemplate;
  checkJournal = "${inputs.skills}/skills/prolog-verification/scripts/check-journal.pl";
  floorsTemplate = ''
    % floors.pl -- minimum clause counts and required entries for the run
    % journal under .prolog/runs/<run>/evidence.pl. Consumed by
    % .prolog/checks/check-journal.pl:
    %
    %   swipl .prolog/checks/check-journal.pl -- .prolog/runs/<run>/evidence.pl
    %
    % SWI recovers past syntax-corrupted terms and keeps loading, so predicate
    % existence alone proves nothing; floors turn "it loaded" into "every
    % expected region of the journal is present". Raise each floor as the
    % journal grows; the gate fails closed when a region goes missing.
    % Uncomment and adjust after the first real append:
    % min_clauses(todo/3, 1).
    % min_clauses(observation/6, 1).
    % required_entry(todo(_, some_task, _)).
  '';
  handoffTemplate = ''
    % handoff.pl -- resumability facts for the next agent session in this
    % directory. Fill each value in as work lands; keep the facts flat, one
    % per line. Format reference: the workspace-root lane at
    % ~/starintel/.prolog/handoff.pl.
    handoff(goal, ''').
    handoff(state, ''').
    handoff(written, ''').
    handoff(next_action, ''').
    handoff(blockers, ''').
    handoff(coordinates, ''').
  '';
  prologBootstrap = pkgs.writeShellApplication {
    name = "prolog-bootstrap";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.swi-prolog
    ];
    text = ''
      set -euo pipefail

      usage() {
        echo "usage: prolog-bootstrap --task <name> [target-dir]" >&2
        exit 2
      }

      task=""
      target="."
      while [ $# -gt 0 ]; do
        case $1 in
          --task)
            [ $# -ge 2 ] || usage
            task=$2
            shift 2
            ;;
          --task=*)
            task=''${1#--task=}
            shift
            ;;
          -h | --help)
            usage
            ;;
          --)
            shift
            break
            ;;
          -*)
            echo "prolog-bootstrap: unknown option $1" >&2
            usage
            ;;
          *)
            target=$1
            shift
            ;;
        esac
      done
      [ -n "$task" ] || {
        echo "prolog-bootstrap: --task is required" >&2
        exit 2
      }
      [ -d "$target" ] || {
        echo "prolog-bootstrap: target directory not found: $target" >&2
        exit 2
      }
      target=$(cd "$target" && pwd)
      cd "$target"

      echo "==> prolog-verify init --task $task"
      prolog-verify init --task "$task"

      mkdir -p .prolog/checks
      install -m 0644 "${checkJournal}" .prolog/checks/check-journal.pl
      [ -e .prolog/checks/floors.pl ] || install -m 0644 "${floorsFile}" .prolog/checks/floors.pl
      [ -e .prolog/handoff.pl ] || install -m 0644 "${handoffFile}" .prolog/handoff.pl

      cat <<'EOF'
      Bootstrap complete: .prolog/facts.kb, verify.pl, checks/{check-journal.pl,floors.pl}, handoff.pl.

      Next steps:
        1. Append work to the run journal under .prolog/runs/<run>/evidence.pl
           (prolog-verify observe -- <command> records machine evidence).
        2. After appending, run the journal gate:
             swipl .prolog/checks/check-journal.pl -- .prolog/runs/<run>/evidence.pl
           exit 0 = intact; exit 1 = corrupt or missing clauses. Raise the
           floors in .prolog/checks/floors.pl as the journal grows.
        3. Huge non-git roots: run prolog-verify from an isolated verifier
           directory (e.g. /tmp/opencode/<task>/) instead of the root, and
           record that evidence path in the journal.
      EOF
    '';
  };
in
{
  options.agentProlog.bootstrap.enable = lib.mkEnableOption "the prolog-bootstrap directory scaffolding CLI";

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.agentVerification.enable or false;
        message = "agentProlog.bootstrap runs prolog-verify from PATH; enable agentVerification alongside it.";
      }
      {
        assertion = builtins.pathExists checkJournal;
        message = "The pinned skills input must export prolog-verification/scripts/check-journal.pl before agentProlog.bootstrap can be enabled.";
      }
      {
        assertion = builtins.pathExists "${inputs.skills}/skills/prolog-verification/SKILL.md";
        message = "The pinned skills input must export the prolog-verification skill before agentProlog.bootstrap can be enabled.";
      }
    ];

    home.packages = [ prologBootstrap ];
  };
}
