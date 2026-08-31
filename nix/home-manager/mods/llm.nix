{ lib, pkgs, inputs, config, ... }:

let
  comfyui = pkgs.comfyui.override { withManager = true; };
  llmLogRevision = "7b1d4359764fcc87fb967ed6e6577a9eaf6f3045";
  llmLogFlake = builtins.getFlake "github:lost-rob0t/llm-log/${llmLogRevision}";
  llmLogPackage = llmLogFlake.packages.${pkgs.stdenv.hostPlatform.system}.default;
  llmLogExpertPackage = llmLogFlake.packages.${pkgs.stdenv.hostPlatform.system}.llm-log-expert;
  llmLogModule = llmLogFlake.homeManagerModules.default;
  proxyBase = "http://127.0.0.1:8787";
in
{
  imports = [
    llmLogModule
    ./brave-mcp.nix
  ];

  options = with lib; {
    llm = {
      enable = mkEnableOption "Enable LLM and zara utils";
    };
  };

  config = with lib; mkIf config.llm.enable {
    # All supported LLM clients use one local, transparent capture plane.
    # The reusable module defaults to XDG_DATA_HOME/llm-log; this machine keeps
    # its long-lived corpus with the rest of the user's AI data instead.
    services.llm-log = {
      enable = true;
      package = llmLogPackage;
      dataDir = "${config.home.homeDirectory}/Documents/AI/proxy";
      expert = {
        enable = true;
        package = llmLogExpertPackage;
        # Tek9 state lives with the proxy corpus on this machine.
        dataDir = "${config.home.homeDirectory}/Documents/AI/proxy/expert";
        require = false;
      };
      upstreams = {
        openai = "https://api.openai.com";
        openrouter = "https://openrouter.ai";
        anthropic = "https://api.anthropic.com";
        chatgpt = "https://chatgpt.com";
      };
    };

    outrunTheme.enable = true;

    # The client modules own packages, complete Home Manager configuration
    # surfaces, MCP integration, and the shared Outrun theme.
    opencode = {
      enable = true;
      llmLog = {
        enable = true;
        baseUrl = proxyBase;
      };
    };
    codex = {
      enable = true;
      llmLog = {
        enable = true;
        baseUrl = proxyBase;
      };
    };
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
    };

    # Brave Search MCP is part of the default LLM tool plane. Authentication
    # remains runtime/user state (`bx config set-key` or BRAVE_SEARCH_API_KEY),
    # so the API key never enters the Nix store.
    braveMcp.enable = mkDefault true;

    # Install required packages for MCP servers and the remaining LLM tools.
    home.packages = with pkgs; [
      inputs.chatgpt-desktop.packages.${stdenv.hostPlatform.system}.default
      opencodeWrapped
      codexWrapped
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
      openai-whisper
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
