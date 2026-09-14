{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.opencode;
  outrun = config.outrunTheme;
  p = outrun.palette;
  webUrl = "http://${cfg.web.hostname}:${toString cfg.web.port}";

  opencodeAttach = pkgs.writeShellApplication {
    name = "opencode-attach";
    text = ''
      exec ${lib.getExe cfg.package} attach ${lib.escapeShellArg webUrl} "$@"
    '';
  };

  opencodeWebOpen = pkgs.writeShellApplication {
    name = "opencode-web-open";
    runtimeInputs = [ pkgs.xdg-utils ];
    text = ''
      exec xdg-open ${lib.escapeShellArg webUrl}
    '';
  };

  commandType = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to install this global command.";
      };
      description = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Optional command-list description.";
      };
      template = mkOption {
        type = types.lines;
        description = "Command prompt body, including literal OpenCode placeholders.";
      };
      agent = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Optional OpenCode agent override.";
      };
      model = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Optional OpenCode model override.";
      };
      subtask = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = "Whether OpenCode should run the command as a subtask.";
      };
      variant = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Optional OpenCode model variant override.";
      };
    };
  };

  validCommandName = name: builtins.match "^[A-Za-z0-9][A-Za-z0-9._-]*$" name != null;
  yamlString = value: builtins.toJSON value;
  renderCommand =
    command:
    let
      metadata =
        lib.optional (command.description != null) "description: ${yamlString command.description}"
        ++ lib.optional (command.agent != null) "agent: ${yamlString command.agent}"
        ++ lib.optional (command.model != null) "model: ${yamlString command.model}"
        ++ lib.optional (command.subtask != null) "subtask: ${builtins.toJSON command.subtask}"
        ++ lib.optional (command.variant != null) "variant: ${yamlString command.variant}";
    in
    lib.concatStringsSep "\n" (
      [ "---" ]
      ++ metadata
      ++ [
        "---"
        ""
        command.template
      ]
    );

  enabledCommands = lib.filterAttrs (_: command: command.enable) cfg.commands;
  commandFiles = lib.mapAttrs' (
    name: command: lib.nameValuePair "opencode/commands/${name}.md" { text = renderCommand command; }
  ) enabledCommands;

  skillsPackages = lib.attrByPath [ "packages" pkgs.stdenv.hostPlatform.system ] { } inputs.skills;
  opencodeWorker = skillsPackages.opencode-worker or null;

  commandSpecs = {
    auto = [
      "Autonomously accomplish a goal"
      "opencode-orchestrate"
    ];
    plan = [
      "Develop an executable, primarily read-only plan"
      "opencode-orchestrate"
    ];
    do = [
      "Execute the established plan"
      "opencode-orchestrate"
    ];
    continue = [
      "Discover state and continue the next coherent slice"
      "opencode-orchestrate"
    ];
    status = [
      "Report concise real execution state"
      "opencode-orchestrate"
    ];
    next = [
      "Select the highest-value coherent next work"
      "opencode-orchestrate"
    ];
    bootstrap = [
      "Bootstrap understanding of an unfamiliar repository"
      "opencode-orchestrate"
    ];
    em = [
      "Open substantial output in the running Emacs client"
      "opencode-orchestrate"
    ];
    rage = [
      "Run the canonical RAGE workflow"
      "rage"
    ];
    auto-rage = [
      "Run bounded iterative RAGE"
      "rage"
    ];
    aradr = [
      "Run the repository's actual ADADR or ARADR workflow"
      "adadr"
    ];
    star-aradr = [
      "Run the StarIntel repository research pipeline"
      "starintel-osint"
    ];
    grill = [
      "Aggressively challenge a target with evidence"
      "grill"
    ];
    skills = [
      "List global, project-local, and relevant skills"
      "skill-scope"
    ];
    skills-new = [
      "Create a reusable local or global skill"
      "skill-scope"
    ];
    skill-edit = [
      "Improve an existing local or global skill"
      "skill-edit"
    ];
    skill-skills-edit = [
      "Alias for skill-edit"
      "skill-edit"
    ];
    commands = [
      "List global, project-local, and built-in commands"
      "opencode-orchestrate"
    ];
    issue = [
      "Find or create a non-duplicate issue"
      "git"
    ];
    issue-next = [
      "Select the next actionable issue"
      "rage"
    ];
    triage = [
      "Triage issues and pull requests safely"
      "git"
    ];
    unfuck = [
      "Repair a path end-to-end with Astra Medium"
      "opencode-worker"
    ];
    ygni = [
      "Debloat while preserving required behavior"
      "ponytail"
    ];
    refactor = [
      "Perform a behavior-preserving refactor"
      "ponytail"
    ];
    review = [
      "Perform a normal engineering review"
      "grill"
    ];
    adversarial-review = [
      "Try to break the implementation"
      "grill"
    ];
    test = [
      "Determine and run appropriate tests"
      "opencode-orchestrate"
    ];
    test-related = [
      "Run the smallest responsible affected tests"
      "opencode-orchestrate"
    ];
    test-adversarial = [
      "Generate and run pathological tests"
      "grill"
    ];
    verify = [
      "Prove current work meets its requirements"
      "prolog-verification"
    ];
    lint = [
      "Run and repair relevant static checks"
      "opencode-orchestrate"
    ];
    bench = [
      "Benchmark against a responsible baseline"
      "opencode-orchestrate"
    ];
    regression = [
      "Reproduce, regress, fix, and verify a bug"
      "opencode-orchestrate"
    ];
    ci = [
      "Run local CI equivalents and inspect remote gates"
      "opencode-orchestrate"
    ];
    ci-fix = [
      "Repair CI failures until green or blocked"
      "opencode-orchestrate"
    ];
    branch = [
      "Create or select an appropriate branch"
      "git"
    ];
    worktree = [
      "Create an isolated worktree"
      "git-worktrees"
    ];
    commit = [
      "Verify and commit only intentional changes"
      "git"
    ];
    pr = [
      "Create or work on the relevant pull request"
      "git"
    ];
    merge = [
      "Merge the exact verified pull-request head"
      "git"
    ];
    merge-on-green = [
      "Wait for and merge the exact green head"
      "merge-on-green"
    ];
    sync-remotes = [
      "Report and safely synchronize configured remotes"
      "git"
    ];
    cleanup = [
      "Clean only merged and inactive temporary state"
      "git-worktrees"
    ];
    diff = [
      "Explain current changes semantically"
      "opencode-orchestrate"
    ];
    research = [
      "Perform bounded engineering research"
      "opencode-orchestrate"
    ];
    deep-research = [
      "Perform deep multi-worker research"
      "worker-orchestration"
    ];
    design = [
      "Produce an implementation-grade design"
      "adadr"
    ];
    spec = [
      "Produce a testable specification"
      "spec"
    ];
    threat-model = [
      "Threat-model the stated goal"
      "grill"
    ];
    compare = [
      "Compare alternatives against project requirements"
      "adadr"
    ];
    prototype = [
      "Build the smallest disposable experiment"
      "opencode-orchestrate"
    ];
    ingest = [
      "Invoke the current project's ingestion path"
      "starintel-ingest"
    ];
    auto-dig = [
      "Use the actual Auto-Dig system"
      "starintel-auto-dig"
    ];
    extract = [
      "Extract schema-conformant records with provenance"
      "starintel-document-create"
    ];
    enumerate = [
      "Breadth-first enumerate the relevant corpus"
      "starintel-osint"
    ];
    expand = [
      "Expand an entity through project relationships"
      "starintel-auto-dig"
    ];
    dataset = [
      "Create or validate a project dataset"
      "starintel-document-create"
    ];
    memory = [
      "Query actual project and global memory"
      "prolog-project-kb"
    ];
    remember = [
      "Persist a reusable verified fact"
      "prolog-project-kb"
    ];
    forget = [
      "Deprecate or remove memory safely"
      "prolog-project-kb"
    ];
    learn = [
      "Promote worthwhile session knowledge"
      "prolog-project-kb"
    ];
    handoff = [
      "Create a machine-useful Org handoff"
      "opencode-orchestrate"
    ];
    resume = [
      "Resume from verified handoff state"
      "opencode-orchestrate"
    ];
    logs = [
      "Gather and analyze relevant logs"
      "debug-system"
    ];
    service = [
      "Inspect service state, health, and dependencies"
      "debug-system"
    ];
    deploy = [
      "Use and verify the project's deployment path"
      "opencode-orchestrate"
    ];
    rollback = [
      "Use the project's supported rollback"
      "opencode-orchestrate"
    ];
    release = [
      "Execute project release gates"
      "opencode-orchestrate"
    ];
    doctor = [
      "Diagnose the development or runtime environment"
      "debug-system"
    ];
    cost = [
      "Report available usage without fabrication"
      "worker-orchestration"
    ];
    budget = [
      "Set a bounded autonomous-work budget"
      "worker-orchestration"
    ];
    model = [
      "Select an actually configured model"
      "opencode-worker"
    ];
    route = [
      "Route a goal to skill, role, model, and mechanism"
      "worker-orchestration"
    ];
    delegate = [
      "Delegate a bounded task through opencode-worker"
      "worker-orchestration"
    ];
    fleet = [
      "Run bounded independent worker lanes"
      "worker-orchestration"
    ];
    reviewer = [
      "Launch an independent reviewer"
      "worker-orchestration"
    ];
    stop = [
      "Stop autonomous work at a safe boundary"
      "opencode-orchestrate"
    ];
    panic = [
      "Immediately stop mutation without destroying state"
      "opencode-orchestrate"
    ];
  };

  modelWorkerCommands = [
    "continue"
    "rage"
    "auto-rage"
    "aradr"
    "star-aradr"
    "grill"
    "unfuck"
    "deep-research"
    "adversarial-review"
    "reviewer"
    "delegate"
    "fleet"
    "route"
  ];

  defaultCommands = lib.mapAttrs (name: spec: {
    description = builtins.elemAt spec 0;
    template = ''
      Load and follow the `${builtins.elemAt spec 1}` skill for this command. Apply it to the complete goal: $ARGUMENTS.${lib.optionalString (builtins.elem name modelWorkerCommands) " Any child whose model, provider, version, variant, or agent profile is selected MUST be launched through the shared `opencode-worker` executable; never bake that selection into this command."}
    '';
  }) commandSpecs;

  theme = {
    "$schema" = "https://opencode.ai/theme.json";
    defs = p;
    theme = {
      primary = "pink";
      secondary = "cyan";
      accent = "ice";
      error = "red";
      warning = "orange";
      success = "green";
      info = "electricBlue";
      text = "foreground";
      textMuted = "muted";
      background = "deepBackground";
      backgroundPanel = "background";
      backgroundElement = "background";
      border = "muted";
      borderActive = "pink";
      borderSubtle = "purple";
      diffAdded = "green";
      diffRemoved = "red";
      diffContext = "muted";
      diffHunkHeader = "cyan";
      diffHighlightAdded = "green";
      diffHighlightRemoved = "red";
      diffAddedBg = "background";
      diffRemovedBg = "background";
      diffContextBg = "deepBackground";
      diffLineNumber = "muted";
      diffAddedLineNumberBg = "background";
      diffRemovedLineNumberBg = "background";
      markdownText = "foreground";
      markdownHeading = "pink";
      markdownLink = "cyan";
      markdownLinkText = "ice";
      markdownCode = "green";
      markdownBlockQuote = "muted";
      markdownEmph = "orange";
      markdownStrong = "yellow";
      markdownHorizontalRule = "purple";
      markdownListItem = "pink";
      markdownListEnumeration = "cyan";
      markdownImage = "violet";
      markdownImageText = "ice";
      markdownCodeBlock = "foreground";
      syntaxComment = "muted";
      syntaxKeyword = "pink";
      syntaxFunction = "cyan";
      syntaxVariable = "ice";
      syntaxString = "green";
      syntaxNumber = "orange";
      syntaxType = "violet";
      syntaxOperator = "pink";
      syntaxPunctuation = "foreground";
    };
  };
in
{
  options.opencode = {
    enable = mkEnableOption "OpenCode with dotfiles policy and integrations";

    package = mkOption {
      type = types.package;
      default = pkgs.opencode;
      defaultText = lib.literalExpression "pkgs.opencode";
      description = "Underlying OpenCode package.";
    };

    globalAgentsFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Optional source for the user-global OpenCode AGENTS.md file.";
    };

    globalSkills = {
      installedSource = mkOption {
        type = types.path;
        default = inputs.skills;
        defaultText = lib.literalExpression "inputs.skills";
        readOnly = true;
        description = "Pinned global skill source installed by Home Manager.";
      };
      sourceCheckout = mkOption {
        type = types.str;
        default = "/home/unseen/Documents/AI/skills";
        description = "Explicit editable checkout corresponding to the installed global skills.";
      };
    };

    commands = mkOption {
      type = types.attrsOf commandType;
      description = "Declarative user-global OpenCode commands.";
      default = defaultCommands // {
        skills.template = ''
          Load and follow the `skill-scope` skill. List project-local skills from the current repository's .opencode/skills and .agents/skills separately from global skills installed from ${cfg.globalSkills.installedSource}; the editable global checkout is ${cfg.globalSkills.sourceCheckout}. Apply any filter in $ARGUMENTS.
        '';
        skills-new.template = ''
          Load and follow the `skill-scope` skill to create $ARGUMENTS. Use project-local .opencode/skills or .agents/skills unless global scope is explicit; global source is ${cfg.globalSkills.sourceCheckout}. Never edit ${cfg.globalSkills.installedSource}.
        '';
        skill-edit.template = ''
          Load and follow the `skill-edit` and `skill-scope` skills for $ARGUMENTS. Resolve local and global origins explicitly; edit global skills only in ${cfg.globalSkills.sourceCheckout}, never ${cfg.globalSkills.installedSource}.
        '';
        skill-skills-edit.template = defaultCommands.skill-edit.template;
        unfuck.template = ''
          Load and follow the `opencode-worker` and `opencode-orchestrate` skills to repair $ARGUMENTS end-to-end. Resolve the logical model `astra-medium` through the shared resolver, then invoke the specialist only through `opencode-worker`; do not invent or hardcode a model ID. The parent retains repository orchestration.
        '';
      };
    };

    llmLog = {
      enable = mkEnableOption "routing OpenCode providers through llm-log";
      baseUrl = mkOption {
        type = types.str;
        default = "http://127.0.0.1:8787";
        description = "Base URL of the local llm-log proxy.";
      };
    };

    web = {
      enable = mkEnableOption "persistent OpenCode web service and desktop launcher";

      hostname = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "Hostname used by the managed OpenCode web service.";
      };

      port = mkOption {
        type = types.port;
        default = 4096;
        description = "TCP port used by the managed OpenCode web service.";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = map (name: {
      assertion = validCommandName name;
      message = "opencode.commands command name `${name}` is unsafe; use one flat alphanumeric name with only `.`, `_`, or `-` separators";
    }) (builtins.attrNames cfg.commands);

    xdg.configFile = commandFiles;
    home.packages =
      lib.optional (opencodeWorker != null) opencodeWorker
      ++ lib.optionals cfg.web.enable [
        opencodeAttach
        opencodeWebOpen
      ];
    home.sessionVariables.OPENCODE_GLOBAL_SKILLS_CHECKOUT = cfg.globalSkills.sourceCheckout;

    home.file."${config.xdg.configHome}/opencode/AGENTS.md" = mkIf (cfg.globalAgentsFile != null) {
      source = cfg.globalAgentsFile;
    };

    xdg.desktopEntries.opencode-web = mkIf cfg.web.enable {
      name = "OpenCode Web";
      genericName = "AI Coding Workspace";
      comment = "Open the persistent local OpenCode web workspace";
      exec = "${opencodeWebOpen}/bin/opencode-web-open";
      icon = "applications-development";
      terminal = false;
      categories = [ "Development" ];
    };

    programs.opencode = {
      enable = true;
      package = cfg.package;
      enableMcpIntegration = true;
      settings = {
        provider = mkIf cfg.llmLog.enable {
          openai.options.baseURL = "${cfg.llmLog.baseUrl}/openai/v1";
          openrouter.options.baseURL = "${cfg.llmLog.baseUrl}/openrouter/api/v1";
          anthropic.options.baseURL = "${cfg.llmLog.baseUrl}/anthropic";
        };
        server = mkIf cfg.web.enable {
          hostname = cfg.web.hostname;
          port = cfg.web.port;
        };
      };
      web = mkIf cfg.web.enable {
        enable = true;
        extraArgs = [
          "--hostname"
          cfg.web.hostname
          "--port"
          (toString cfg.web.port)
        ];
      };
      themes.${outrun.name} = mkIf outrun.enable theme;
      tui.theme = mkIf outrun.enable outrun.name;
    };
  };
}
