{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.agentVerification;
  policyFile = ../files/global-agents.md;
  verifierScript = "${inputs.skills}/skills/prolog-verification/scripts/prolog-verify.py";
  verifier = pkgs.writeShellApplication {
    name = "prolog-verify";
    runtimeInputs = [
      pkgs.brave-search-cli
      pkgs.git
      pkgs.python3
      pkgs.swi-prolog
    ];
    text = ''
      exec ${pkgs.python3}/bin/python3 ${verifierScript} "$@"
    '';
  };
  codexHooks = {
    description = "Global durable Prolog verification gate";
    hooks = {
      SessionStart = [
        {
          matcher = "startup|resume|clear|compact";
          hooks = [
            {
              type = "command";
              command = "${verifier}/bin/prolog-verify hook-session-start";
              timeout = 15;
              statusMessage = "Recording verification baseline";
            }
          ];
        }
      ];
      Stop = [
        {
          hooks = [
            {
              type = "command";
              command = "${verifier}/bin/prolog-verify hook-stop";
              timeout = 60;
              statusMessage = "Checking Prolog verification";
            }
          ];
        }
      ];
    };
  };
in
{
  options.agentVerification.enable = lib.mkEnableOption "global on-disk Prolog verification for coding agents";

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.pathExists verifierScript;
        message = "The pinned skills input must export prolog-verification before agentVerification can be enabled.";
      }
    ];

    home.packages = [ verifier ];

    codex.globalAgentsFile = policyFile;
    opencode.globalAgentsFile = policyFile;

    home.file.".codex/hooks.json".text = builtins.toJSON codexHooks;
  };
}
