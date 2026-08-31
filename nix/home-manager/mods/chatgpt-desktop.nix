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
  cfg = config.programs.chatgpt-desktop;
  outrun = config.outrunTheme;
  p = outrun.palette;
  codexHome =
    if config.home.preferXdgDirectories then
      "${config.xdg.configHome}/codex"
    else
      "${config.home.homeDirectory}/.codex";

  colorType = types.strMatching "^#[0-9a-fA-F]{6}$";
  chromeThemeType = types.submodule {
    options = {
      accent = mkOption { type = colorType; };
      contrast = mkOption { type = types.ints.between 0 100; };
      fonts = {
        code = mkOption { type = types.nullOr types.str; };
        ui = mkOption { type = types.nullOr types.str; };
      };
      ink = mkOption { type = colorType; };
      opaqueWindows = mkOption { type = types.bool; };
      semanticColors = {
        diffAdded = mkOption { type = colorType; };
        diffRemoved = mkOption { type = colorType; };
        skill = mkOption { type = colorType; };
      };
      surface = mkOption { type = colorType; };
    };
  };

  appearancePayload = {
    appearanceTheme = cfg.appearance.mode;
    appearanceLightCodeThemeId = cfg.appearance.lightCodeThemeId;
    appearanceDarkChromeTheme = cfg.appearance.darkChromeTheme;
    appearanceDarkCodeThemeId = cfg.appearance.darkCodeThemeId;
    appearanceDiffMarkerStyle = cfg.appearance.diffMarkerStyle;
    sansFontSize = cfg.appearance.sansFontSize;
    codeFontSize = cfg.appearance.codeFontSize;
    useFontSmoothing = cfg.appearance.useFontSmoothing;
    usePointerCursors = cfg.appearance.usePointerCursors;
  }
  // lib.optionalAttrs (cfg.appearance.lightChromeTheme != null) {
    appearanceLightChromeTheme = cfg.appearance.lightChromeTheme;
  };
  themePatch = pkgs.writeText "chatgpt-desktop-outrun-config.json" (
    builtins.toJSON {
      desktop = appearancePayload;
    }
  );
  themeUpdater = pkgs.writeShellApplication {
    name = "apply-chatgpt-desktop-config";
    runtimeInputs = [
      (pkgs.python3.withPackages (pythonPackages: [ pythonPackages.tomlkit ]))
    ];
    text = ''
      exec python3 ${../files/apply-codex-config.py} "$@"
    '';
  };
in
{
  options.programs.chatgpt-desktop = {
    enable = mkEnableOption "the ChatGPT/Codex desktop application";

    package = mkOption {
      type = types.package;
      default = inputs.chatgpt-desktop.packages.${pkgs.stdenv.hostPlatform.system}.default;
      defaultText = lib.literalExpression "inputs.chatgpt-desktop.packages.<system>.default";
      description = "ChatGPT desktop package to install.";
    };

    appearance = {
      enable = mkOption {
        type = types.bool;
        default = outrun.enable;
        defaultText = lib.literalExpression "config.outrunTheme.enable";
        description = "Manage the desktop application's appearance settings.";
      };
      mode = mkOption {
        type = types.enum [
          "system"
          "light"
          "dark"
        ];
        default = "dark";
        description = "Desktop application appearance mode.";
      };
      lightChromeTheme = mkOption {
        type = types.nullOr chromeThemeType;
        default = null;
        description = "Optional light desktop chrome theme.";
      };
      darkChromeTheme = mkOption {
        type = chromeThemeType;
        default = {
          accent = p.pink;
          contrast = 64;
          fonts = {
            code = outrun.fonts.code;
            ui = outrun.fonts.ui;
          };
          ink = p.foreground;
          opaqueWindows = false;
          semanticColors = {
            diffAdded = p.green;
            diffRemoved = p.red;
            skill = p.cyan;
          };
          surface = p.deepBackground;
        };
        description = "Dark desktop chrome theme stored under [desktop].";
      };
      lightCodeThemeId = mkOption {
        type = types.str;
        default = "codex";
        description = "Built-in desktop syntax theme used in light mode.";
      };
      darkCodeThemeId = mkOption {
        type = types.str;
        default = "aura";
        description = "Built-in desktop syntax theme used with the Outrun chrome.";
      };
      diffMarkerStyle = mkOption {
        type = types.enum [
          "color"
          "symbols"
        ];
        default = "color";
        description = "Diff marker style.";
      };
      sansFontSize = mkOption {
        type = types.ints.between 11 16;
        default = 14;
        description = "Desktop UI font size.";
      };
      codeFontSize = mkOption {
        type = types.ints.between 8 24;
        default = 12;
        description = "Desktop code font size.";
      };
      useFontSmoothing = mkOption {
        type = types.bool;
        default = true;
        description = "Enable font smoothing.";
      };
      usePointerCursors = mkOption {
        type = types.bool;
        default = true;
        description = "Use pointer cursors for interactive controls.";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs.codex.settings = mkIf (config.codex.manageConfig && cfg.appearance.enable) {
      desktop = appearancePayload;
    };

    home.file.".codex/themes/${outrun.name}-desktop.json" = mkIf cfg.appearance.enable {
      text = builtins.toJSON appearancePayload;
    };

    home.activation.chatgptDesktopOutrunTheme =
      mkIf (cfg.appearance.enable && !config.codex.manageConfig)
        (
          lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            $DRY_RUN_CMD ${themeUpdater}/bin/apply-chatgpt-desktop-config \
              ${lib.escapeShellArg "${codexHome}/config.toml"} \
              ${themePatch}
          ''
        );
  };
}
