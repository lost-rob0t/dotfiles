{
  config,
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
    home.file."${config.xdg.configHome}/opencode/AGENTS.md" = mkIf (cfg.globalAgentsFile != null) {
      source = cfg.globalAgentsFile;
    };

    home.packages = lib.optionals cfg.web.enable [
      opencodeAttach
      opencodeWebOpen
    ];

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
