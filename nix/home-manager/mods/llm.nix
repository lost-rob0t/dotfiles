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
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-dictate
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-wake
      inputs.zara.packages.${stdenv.hostPlatform.system}.zara-prolog
      inputs.org-vector.packages.${stdenv.hostPlatform.system}.org-vector
      nodejs_22
      claude-code
      playerctl
      pavucontrol
      pulseaudio

      espeak-ng
      sox
      alsa-utils

      # GUI and notification utilities
      dunst
      libnotify
      scrot
      brightnessctl
      i3lock

      jq
      curl

      # Python with packages for AI assistant
      (python3.withPackages (ps: with ps; [
        requests
        websockets
        tkinter
      ]))

      openai-whisper
    ];

    # Environment variables for MCP

    # Auto-start MCP servers (optional)
    # home.activation.startMcpServers = lib.hm.dag.entryAfter ["writeBoundary"] ''
    #   $DRY_RUN_CMD ${pkgs.bash}/bin/bash $HOME/.local/bin/start-mcp-servers start
    # '';
  };
}
