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

  opencodeYoloRuntime = pkgs.writeShellApplication {
    name = "opencode-yolo-runtime";
    runtimeInputs = [ pkgs.coreutils ];
    text = builtins.readFile ../files/opencode-yolo.sh;
  };

  wrappedPackage =
    (pkgs.writeShellScriptBin "opencode" ''
      export OPENCODE_REAL_BIN="${cfg.package}/bin/opencode"
      exec "${opencodeYoloRuntime}/bin/opencode-yolo-runtime" "$@"
    '').overrideAttrs
      (old: {
        meta = (old.meta or { }) // {
          mainProgram = "opencode";
        };
      });

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

    yolo.enable = mkOption {
      type = types.bool;
      default = true;
      description = "Enable the existing policy wrapper for opencode --yolo.";
    };

    llmLog = {
      enable = mkEnableOption "routing OpenCode providers through llm-log";
      baseUrl = mkOption {
        type = types.str;
        default = "http://127.0.0.1:8787";
        description = "Base URL of the local llm-log proxy.";
      };
    };
  };

  config = mkIf cfg.enable {
    programs.opencode = {
      enable = true;
      package = if cfg.yolo.enable then wrappedPackage else cfg.package;
      enableMcpIntegration = true;
      settings.provider = mkIf cfg.llmLog.enable {
        openai.options.baseURL = "${cfg.llmLog.baseUrl}/openai/v1";
        openrouter.options.baseURL = "${cfg.llmLog.baseUrl}/openrouter/api/v1";
        anthropic.options.baseURL = "${cfg.llmLog.baseUrl}/anthropic";
      };
      themes.${outrun.name} = mkIf outrun.enable theme;
      tui.theme = mkIf outrun.enable outrun.name;
    };
  };
}
