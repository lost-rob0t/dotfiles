{ lib, pkgs, inputs, config, ... }:

{
  options = with lib; {
    llm = {
      enable = mkEnableOption "Enable LLM and zara utils";
    };
  };

  config = with lib; mkIf config.llm.enable {
    # Install required packages for MCP servers
    home.packages = with pkgs; [
      inputs.zara.packages.${stdenv.hostPlatform.system}.zarathushtra
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-cli
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-wake
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-dictate
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-prolog
      inputs.org-vector.packages.${stdenv.hostPlatform.system}.org-vector

      # LLM Editors
      opencode
      claude-code

      # Local generative AI
      (comfyui.override { withManager = true; })

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

    # ComfyUI from nixpkgs uses a writable XDG data directory instead of the
    # immutable Nix store. Keep downloaded models and generated media here.
    home.activation.comfyuiDirectories = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD mkdir -p \
        "$HOME/.local/share/comfyui/models/diffusion_models" \
        "$HOME/.local/share/comfyui/models/text_encoders" \
        "$HOME/.local/share/comfyui/models/vae" \
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
