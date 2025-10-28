# Example configuration showing how to integrate LLM assistant

{ config, pkgs, ... }:

{
  imports = [
    ./llm.nix  # Import the LLM module
  ];

  # Enable the LLM assistant
  services.llm = {
    enable = true;
    whisperModel = "tiny";    # Fast, good for quick commands
    # whisperModel = "base";  # Better accuracy, slightly slower
    recordDuration = 4;       # 4 seconds of recording
  };

  # Optional: Set API keys via environment variables
  home.sessionVariables = {
    # For Anthropic Claude (recommended)
    ANTHROPIC_API_KEY = "your-api-key-here";

    # Or for OpenAI
    # OPENAI_API_KEY = "your-api-key-here";

    # Or load from a secret file
    # ANTHROPIC_API_KEY = builtins.readFile /path/to/secret;
  };

  # Optional: Additional Emacs configuration
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      gptel  # AI assistant
      # ... your other packages
    ];
  };

  # Optional: Enable Emacs daemon for faster startup
  services.emacs = {
    enable = true;
    client.enable = true;
    startWithUserSession = "graphical";
  };

  # Example Qtile integration (if using qtile)
  # You would add this to your qtile config.py:
  #
  # from libqtile.config import Key
  # from libqtile.lazy import lazy
  #
  # keys = [
  #     # AI Voice Assistant
  #     Key([mod, "shift"], "a",
  #         lazy.spawn("qtile-llm-bridge voice"),
  #         desc="Voice AI assistant"),
  #
  #     # AI Chat
  #     Key([mod, "shift"], "c",
  #         lazy.spawn("qtile-llm-bridge chat"),
  #         desc="Open AI chat"),
  #
  #     # Quick Query
  #     Key([mod, "shift"], "q",
  #         lazy.spawn("sh -c 'query=$(echo | rofi -dmenu -p \"Ask AI:\") && qtile-llm-bridge query --query \"$query\"'"),
  #         desc="Quick AI query"),
  # ]
}
