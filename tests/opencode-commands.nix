{
  home-manager,
  inputs,
  lib,
  pkgs,
}:

let
  mkHome =
    extraModule:
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        inputs.skills.homeManagerModules.opencode
        ../nix/home-manager/mods/outrun-theme.nix
        ../nix/home-manager/mods/opencode.nix
        {
          home = {
            username = "command-test";
            homeDirectory = "/home/command-test";
            stateVersion = "25.11";
          };
          opencode = {
            enable = true;
            package = pkgs.hello;
          };
        }
        extraModule
      ];
    };

  fixture = mkHome {
    home.file.".opencode/commands/project-only.md".text = "project command";
    opencode.commands = lib.mkForce {
      simple = {
        description = "Simple command";
        template = "Use the spec skill for $ARGUMENTS and first item $1.";
        agent = "build";
        model = "openai/test";
        subtask = true;
        variant = "high";
      };
      disabled = {
        enable = false;
        template = "must not exist";
      };
    };
  };

  reverseFixture = mkHome {
    opencode.commands = lib.mkForce {
      zed.template = "z";
      alpha.template = "a";
    };
  };
  forwardFixture = mkHome {
    opencode.commands = lib.mkForce {
      alpha.template = "a";
      zed.template = "z";
    };
  };

  unsafeFixture = mkHome {
    opencode.commands = lib.mkForce {
      "../escape".template = "unsafe";
    };
  };

  commandPath = "opencode/commands/simple.md";
  commandText = fixture.config.xdg.configFile.${commandPath}.text;
  unsafeEvaluation = builtins.tryEval unsafeFixture.activationPackage;
  defaults = (mkHome { }).config.opencode.commands;
  expectedCommands = [
    "adversarial-review"
    "aradr"
    "auto"
    "auto-dig"
    "auto-rage"
    "bench"
    "bootstrap"
    "branch"
    "budget"
    "ci"
    "ci-fix"
    "cleanup"
    "commands"
    "commit"
    "compare"
    "continue"
    "cost"
    "dataset"
    "deep-research"
    "delegate"
    "deploy"
    "design"
    "diff"
    "do"
    "doctor"
    "em"
    "enumerate"
    "expand"
    "extract"
    "fleet"
    "forget"
    "grill"
    "handoff"
    "ingest"
    "issue"
    "issue-next"
    "learn"
    "lint"
    "logs"
    "memory"
    "merge"
    "merge-on-green"
    "model"
    "next"
    "panic"
    "plan"
    "pr"
    "prototype"
    "refactor"
    "regression"
    "release"
    "remember"
    "research"
    "resume"
    "review"
    "reviewer"
    "rollback"
    "route"
    "service"
    "skill-edit"
    "skill-skills-edit"
    "skills"
    "skills-new"
    "spec"
    "star-aradr"
    "status"
    "stop"
    "sync-remotes"
    "test"
    "test-adversarial"
    "test-related"
    "threat-model"
    "triage"
    "unfuck"
    "verify"
    "worktree"
    "ygni"
  ];
in
assert builtins.hasAttr commandPath fixture.config.xdg.configFile;
assert lib.hasInfix ''description: "Simple command"'' commandText;
assert lib.hasInfix ''agent: "build"'' commandText;
assert lib.hasInfix ''model: "openai/test"'' commandText;
assert lib.hasInfix "subtask: true" commandText;
assert lib.hasInfix ''variant: "high"'' commandText;
assert lib.hasInfix "$ARGUMENTS" commandText;
assert lib.hasInfix "$1" commandText;
assert !builtins.hasAttr "opencode/commands/disabled.md" fixture.config.xdg.configFile;
assert builtins.hasAttr ".opencode/commands/project-only.md" fixture.config.home.file;
assert
  reverseFixture.config.xdg.configFile."opencode/commands/alpha.md".text
  == forwardFixture.config.xdg.configFile."opencode/commands/alpha.md".text;
assert
  reverseFixture.config.xdg.configFile."opencode/commands/zed.md".text
  == forwardFixture.config.xdg.configFile."opencode/commands/zed.md".text;
assert !unsafeEvaluation.success;
assert builtins.length (builtins.attrNames defaults) == 78;
assert lib.all (name: builtins.hasAttr name defaults) expectedCommands;
assert lib.all (name: lib.hasInfix "for `/${name}`" defaults.${name}.template) expectedCommands;
assert lib.all (name: lib.hasInfix "$ARGUMENTS" defaults.${name}.template) expectedCommands;
assert lib.all (name: !lib.hasInfix "complete goal:" defaults.${name}.template) expectedCommands;
assert lib.hasInfix "Command intent: Report concise real execution state." defaults.status.template;
assert lib.hasInfix "User arguments (may be empty):" defaults.status.template;
assert lib.hasInfix "Do not invent a goal when no arguments were supplied." defaults.status.template;
assert lib.hasInfix "opencode-worker" defaults.unfuck.template;
assert lib.hasInfix "astra-medium" defaults.unfuck.template;
assert !lib.hasInfix "openai/gpt-6-astra" defaults.unfuck.template;
assert lib.all (name: lib.hasInfix "opencode-worker" defaults.${name}.template) [
  "continue"
  "rage"
  "auto-rage"
  "aradr"
  "star-aradr"
  "grill"
  "deep-research"
  "adversarial-review"
  "reviewer"
  "delegate"
  "fleet"
  "route"
];
assert lib.hasInfix "/home/unseen/Documents/AI/skills" defaults.skills-new.template;
pkgs.runCommand "opencode-commands-check" { } ''
  touch "$out"
''