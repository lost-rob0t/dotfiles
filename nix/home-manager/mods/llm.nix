{ lib, pkgs, inputs, config, ... }:

let
  comfyui = pkgs.comfyui.override { withManager = true; };
  llmLogRevision = "692d3820f638c011fc248a7694ecabd72a6e2895";
  llmLogFlake = builtins.getFlake "github:lost-rob0t/llm-log/${llmLogRevision}";
  llmLogPackage = llmLogFlake.packages.${pkgs.stdenv.hostPlatform.system}.default;
  llmLogExpertPackage = llmLogFlake.packages.${pkgs.stdenv.hostPlatform.system}.llm-log-expert;
  llmLogModule = llmLogFlake.homeManagerModules.default;
  proxyBase = "http://127.0.0.1:8787";
  llmLogDataDir = "${config.home.homeDirectory}/Documents/AI/proxy";
  llmLogCorpus = "${llmLogDataDir}/events.jsonl";
  llmLogExpertAdmin = "http://127.0.0.1:8788";

  llmLogQuery = pkgs.writeShellApplication {
    name = "llm-log-query";
    text = ''
      exec ${llmLogPackage}/bin/llm-log expert \
        --admin-url ${lib.escapeShellArg llmLogExpertAdmin} \
        query "$@"
    '';
  };

  llmLogBackfill = pkgs.writeShellApplication {
    name = "llm-log-backfill";
    text = ''
      exec ${llmLogPackage}/bin/llm-log expert \
        --admin-url ${lib.escapeShellArg llmLogExpertAdmin} \
        backfill --source ${lib.escapeShellArg llmLogCorpus} "$@"
    '';
  };

  llmLogExport = pkgs.writeShellApplication {
    name = "llm-log-export";
    text = ''
      exec ${llmLogPackage}/bin/llm-log expert \
        --admin-url ${lib.escapeShellArg llmLogExpertAdmin} \
        export-dataset --source ${lib.escapeShellArg llmLogCorpus} "$@"
    '';
  };

  # Temporary workaround for NixOS/nixpkgs#563241, matching the fix merged
  # upstream in NixOS/nixpkgs#564101. Bun 1.4.x executable code splitting can
  # produce an OpenCode binary that crashes in SystemPrompt.environment before
  # provider dispatch. Keep this local until the pinned nixpkgs includes #564101.
  opencodePackage = pkgs.opencode.overrideAttrs (oldAttrs: {
    postPatch = (oldAttrs.postPatch or "") + ''
      # Bun 1.4.x regression: compiled executable code splitting breaks OpenCode.
      substituteInPlace packages/opencode/script/build.ts \
        --replace-fail 'splitting: true,' 'splitting: false,'
    '';
    passthru = (oldAttrs.passthru or { }) // {
      promptSendWorkaround = "NixOS/nixpkgs#564101";
    };
  });
  youtubeContext = pkgs.writeShellApplication {
    name = "youtube-context";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      ffmpeg
      openai-whisper
      python3
      yt-dlp
    ];
    text = ''
      exec ${inputs.skills}/skills/youtube-context/scripts/youtube-context "$@"
    '';
  };
in
{
  imports = [
    llmLogModule
    ./brave-mcp.nix
    ./llm-log-quant-alerts.nix
    ./starintel-llm-harness.nix
  ];

  options = with lib; {
    llm = {
      enable = mkEnableOption "Enable LLM and zara utils";
      quotaTelemetry = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Collect provider-reported z.AI and GPT quota metadata for Qtile.";
        };
        environmentFile = mkOption {
          type = types.str;
          default = "${config.xdg.configHome}/llm-log/quotas.env";
          description = "Optional private runtime environment file; never put API keys in Nix values.";
        };
      };
      expertBackfill = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Incrementally replay new capture events into the live Tek9 expert plane.";
        };
        interval = mkOption {
          type = types.str;
          default = "15m";
          description = "systemd OnUnitActiveSec interval for checkpointed llm-log expert backfill.";
        };
      };
    };
  };

  config = with lib; mkIf config.llm.enable {
    # All supported LLM clients use one local, transparent capture plane.
    # The reusable module defaults to XDG_DATA_HOME/llm-log; this machine keeps
    # its long-lived corpus with the rest of the user's AI data instead.
    services.llm-log = {
      enable = true;
      package = llmLogPackage;
      dataDir = llmLogDataDir;
      expert = {
        enable = true;
        package = llmLogExpertPackage;
        # Tek9 state lives with the proxy corpus on this machine.
        dataDir = "${llmLogDataDir}/expert";
        require = false;
      };
      upstreams = {
        openai = "https://api.openai.com";
        openrouter = "https://openrouter.ai";
        anthropic = "https://api.anthropic.com";
        chatgpt = "https://chatgpt.com";
      };
    };

    # Read-only quota observations use existing managed auth. Bypass the Codex
    # capture wrapper so telemetry requests cannot feed back into token history.
    # The optional file supplies ZAI_API_KEY or LLM_LOG_ZAI_KEY_FILE at runtime.
    systemd.user.services.llm-log.Service = mkIf config.llm.quotaTelemetry.enable {
      Environment = lib.mkAfter [
        "LLM_LOG_QUOTAS_ENABLED=1"
        "LLM_LOG_CODEX_BIN=${config.codex.package}/bin/codex"
        "CODEX_HOME=${if config.home.preferXdgDirectories then "${config.xdg.configHome}/codex" else "${config.home.homeDirectory}/.codex"}"
      ];
      EnvironmentFile = lib.mkAfter [ "-${config.llm.quotaTelemetry.environmentFile}" ];
    };

    # Replay only the append-only suffix that has not already crossed the
    # checkpoint. The llm-log backfill implementation streams JSONL one record
    # at a time; this unit never materializes the corpus in memory.
    systemd.user.services.llm-log-expert-backfill = mkIf config.llm.expertBackfill.enable {
      Unit = {
        Description = "Incrementally backfill llm-log capture into Tek9";
        After = [ "llm-log.service" ];
        Requires = [ "llm-log.service" ];
        ConditionPathExists = llmLogCorpus;
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${llmLogBackfill}/bin/llm-log-backfill";
        TimeoutStartSec = "15m";
        Nice = 10;
        IOSchedulingClass = "best-effort";
        IOSchedulingPriority = 7;
      };
    };

    systemd.user.timers.llm-log-expert-backfill = mkIf config.llm.expertBackfill.enable {
      Unit.Description = "Schedule incremental llm-log expert backfill";
      Timer = {
        OnBootSec = "5m";
        OnUnitActiveSec = config.llm.expertBackfill.interval;
        Persistent = true;
        Unit = "llm-log-expert-backfill.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };

    outrunTheme.enable = true;
    llm.starintelHarness.enable = true;

    # The client modules own packages, complete Home Manager configuration
    # surfaces, MCP integration, and the shared Outrun theme.
    opencode = {
      enable = true;
      package = opencodePackage;
      llmLog = {
        enable = true;
        baseUrl = proxyBase;
      };
      commands = {
        llm-log-query = {
          description = "Run a bounded read-only llm-log expert query";
          template = ''
            Use the Home Manager-installed `llm-log-query` command to inspect
            the running llm-log expert plane.
            User arguments:
            $ARGUMENTS
            Never read events.jsonl directly. Keep every query bounded and use
            the declared expert operations rather than inventing Prolog goals.
          '';
        };
        llm-log-backfill = {
          description = "Run or inspect checkpointed llm-log expert backfill";
          template = ''
            Use the Home Manager-installed `llm-log-backfill` command.
            User arguments:
            $ARGUMENTS
            Preserve checkpointed streaming replay. Never copy or load the full
            corpus into memory and do not launch a second Tek9 owner.
          '';
        };
        llm-log-export = {
          description = "Export a bounded provenance-preserving llm-log dataset";
          template = ''
            Use the Home Manager-installed `llm-log-export` command.
            User arguments:
            $ARGUMENTS
            Never read events.jsonl directly. Use paginated expert selection and
            let llm-log join only the selected event IDs back to raw evidence.
          '';
        };
      };
    };
    codex = {
      enable = true;
      llmLog = {
        enable = true;
        baseUrl = proxyBase;
      };
    };
    agentVerification.enable = true;
    programs.chatgpt-desktop.enable = true;

    # gptel backends are created lazily by Doom. Advice the OpenRouter
    # constructor instead of replacing the user's configured backend/model
    # lists, API-key lookup or interactive model switching.
    programs.emacs.extraConfig = lib.mkAfter ''
      (defun nsa/llm-log--gptel-openrouter-args (args)
        (let ((name (car args))
              (options (copy-sequence (cdr args))))
          (if (equal (plist-get options :host) "openrouter.ai")
              (progn
                (setq options (plist-put options :host "127.0.0.1:8787"))
                (setq options (plist-put options :protocol "http"))
                (setq options
                      (plist-put options :endpoint
                                 "/openrouter/api/v1/chat/completions"))
                (cons name options))
            args)))

      (with-eval-after-load 'gptel-openai
        (unless (advice-member-p #'nsa/llm-log--gptel-openrouter-args
                                 'gptel-make-openai)
          (advice-add 'gptel-make-openai
                      :filter-args #'nsa/llm-log--gptel-openrouter-args)))
    '';

    # Claude Code officially supports ANTHROPIC_BASE_URL for LLM gateways.
    # Keep credentials in Claude's normal auth path; llm-log only sees and
    # forwards them, while persisting a redacted header copy.
    home.sessionVariables = {
      STARINTEL_URL = mkDefault "http://127.0.0.1:5000";
      STARINTEL_TIMEOUT_SECONDS = mkDefault "10";
      ANTHROPIC_BASE_URL = mkDefault "${proxyBase}/anthropic";
      LLM_LOG_BASE_URL = mkDefault proxyBase;
      LLM_LOG_API_URL = mkDefault proxyBase;
      LLM_LOG_CORPUS = mkDefault llmLogCorpus;
      LLM_LOG_EXPERT_ADMIN_URL = mkDefault llmLogExpertAdmin;
      LLM_LOG_EXPERT_DATA_DIR = mkDefault "${llmLogDataDir}/expert";
      # Reserved for the bounded KB miner tracked in llm-log#102. The model ID
      # is deliberately discovered at runtime instead of guessed in Nix.
      LLM_LOG_LEARN_BASE_URL = mkDefault "https://llm.starintel.actor";
      LLM_LOG_LEARN_MODEL_SELECTOR = mkDefault "auto:27b";
    };

    # Brave Search MCP is part of the default LLM tool plane. Authentication
    # remains runtime/user state (`bx config set-key` or BRAVE_SEARCH_API_KEY),
    # so the API key never enters the Nix store.
    braveMcp.enable = mkDefault true;

    # Install required packages for MCP servers and the remaining LLM tools.
    home.packages = with pkgs; [
      claude-code

      # Local generative AI
      comfyui

      playerctl
      pavucontrol
      pulseaudio
      espeak-ng
      sox
      ffmpeg
      alsa-utils

      # GUI and notification utilities
      dunst
      libnotify
      scrot
      brightnessctl
      i3lock

      jq
      curl
      llmLogQuery
      llmLogBackfill
      llmLogExport
      openai-whisper
      youtubeContext
    ];

    # ComfyUI uses a writable XDG data directory instead of the immutable
    # Nix store. Keep downloaded models and generated media here.
    home.activation.comfyuiDirectories = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir -p \
        "$HOME/.local/share/comfyui/models/diffusion_models" \
        "$HOME/.local/share/comfyui/models/text_encoders" \
        "$HOME/.local/share/comfyui/models/vae" \
        "$HOME/.local/share/comfyui/models/checkpoints" \
        "$HOME/.local/share/comfyui/models/loras" \
        "$HOME/.local/share/comfyui/custom_nodes" \
        "$HOME/.local/share/comfyui/input" \
        "$HOME/.local/share/comfyui/output" \
        "$HOME/.local/share/comfyui/user"
    '';

    # Environment variables for MCP

    # Auto-start MCP servers (optional)
    # home.activation.startMcpServers = lib.hm.dag.entryAfter ["writeBoundary"] ''
    #   $DRY_RUN_CMD ${pkgs.bash}/bin/bash $HOME/.local/bin/start-mcp-servers start
    # '';
  };
}
