{ lib, pkgs, inputs, config, ... }:

let
  comfyui = pkgs.comfyui.override { withManager = true; };

  # Keep the OpenCode policy wrapper in dotfiles, not in the reusable skills
  # repository.  Normal invocations are passed through unchanged; only
  # `opencode --yolo` creates the StarIntel V4 tmpfs/Prolog-RLM environment.
  opencodeYoloRuntime = pkgs.writeShellApplication {
    name = "opencode-yolo-runtime";
    runtimeInputs = [ pkgs.coreutils ];
    text = builtins.readFile ../files/opencode-yolo.sh;
  };

  # home-manager's programs.opencode module needs meta.mainProgram when it
  # wraps or references the package (lib.getExe, extraPackages wrapping).
  opencodeWrapped = (pkgs.writeShellScriptBin "opencode" ''
    export OPENCODE_REAL_BIN="${pkgs.opencode}/bin/opencode"
    exec "${opencodeYoloRuntime}/bin/opencode-yolo-runtime" "$@"
  '').overrideAttrs (old: {
    meta = (old.meta or { }) // { mainProgram = "opencode"; };
  });
in
{
  imports = [
    ./brave-mcp.nix
  ];

  options = with lib; {
    llm = {
      enable = mkEnableOption "Enable LLM and zara utils";
    };
  };

  config = with lib; mkIf config.llm.enable {
    # Brave Search MCP is part of the default LLM tool plane. Authentication
    # remains runtime/user state (`bx config set-key` or BRAVE_SEARCH_API_KEY),
    # so the API key never enters the Nix store.
    braveMcp.enable = mkDefault true;

    # Route the programs.opencode module through the policy wrapper so raw
    # pkgs.opencode and the wrapper never both land in home.packages (their
    # bin/opencode entries collide in the home-manager-path buildEnv).
    programs.opencode.package = mkDefault opencodeWrapped;

    # Install required packages for MCP servers
    home.packages = with pkgs; [
      inputs.zara.packages.${stdenv.hostPlatform.system}.zarathushtra
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-cli
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-wake
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-dictate
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-prolog
      # LLM Editors and desktop clients
      inputs.chatgpt-desktop.packages.${stdenv.hostPlatform.system}.default
      opencodeWrapped
      claude-code

      # Local generative AI
      comfyui

      playerctl
      pavucontrol
      pulseaudio
      #codex
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

    # Zara loads user tools from ~/.zarathushtra/plugins at startup.
    home.file.".zarathushtra/plugins/starintel.py".source =
      ../files/zarathushtra/plugins/starintel.py;

    # Local-only StarIntel is the safe/default deployment. Override this in a
    # host config when Zara should talk to another trusted StarIntel instance.
    home.sessionVariables.STARINTEL_URL = mkDefault "http://127.0.0.1:5000";
    home.sessionVariables.STARINTEL_TIMEOUT_SECONDS = mkDefault "10";

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
