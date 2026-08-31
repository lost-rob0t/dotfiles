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
  cfg = config.codex;
  outrun = config.outrunTheme;
  p = outrun.palette;
  codexHome =
    if config.home.preferXdgDirectories then
      "${config.xdg.configHome}/codex"
    else
      "${config.home.homeDirectory}/.codex";

  skillsSource = inputs.skills;
  availableSkills = builtins.attrNames (
    lib.filterAttrs (_: entryType: entryType == "directory") (builtins.readDir "${skillsSource}/skills")
  );

  wrappedPackage =
    (pkgs.writeShellScriptBin "codex" ''
      exec "${cfg.package}/bin/codex" \
        -c 'openai_base_url="${cfg.llmLog.baseUrl}/openai/v1"' \
        -c 'chatgpt_base_url="${cfg.llmLog.baseUrl}/chatgpt/backend-api"' \
        "$@"
    '').overrideAttrs
      (_: {
        inherit (cfg.package) version;
        meta = (cfg.package.meta or { }) // {
          mainProgram = "codex";
        };
      });

  themePatch = pkgs.writeText "codex-outrun-config.json" (
    builtins.toJSON {
      tui.theme = outrun.name;
    }
  );

  themeUpdater = pkgs.writeShellApplication {
    name = "apply-codex-config";
    runtimeInputs = [
      (pkgs.python3.withPackages (pythonPackages: [ pythonPackages.tomlkit ]))
    ];
    text = ''
      exec python3 ${../files/apply-codex-config.py} "$@"
    '';
  };

  tmTheme = ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>name</key>
      <string>Outrun</string>
      <key>settings</key>
      <array>
        <dict>
          <key>settings</key>
          <dict>
            <key>background</key><string>${p.deepBackground}</string>
            <key>caret</key><string>${p.cyan}</string>
            <key>foreground</key><string>${p.foreground}</string>
            <key>invisibles</key><string>${p.muted}</string>
            <key>lineHighlight</key><string>${p.background}</string>
            <key>selection</key><string>${p.purple}</string>
          </dict>
        </dict>
        <dict>
          <key>name</key><string>Comments</string>
          <key>scope</key><string>comment, punctuation.definition.comment</string>
          <key>settings</key><dict><key>foreground</key><string>${p.muted}</string><key>fontStyle</key><string>italic</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Strings</string>
          <key>scope</key><string>string, constant.other.symbol</string>
          <key>settings</key><dict><key>foreground</key><string>${p.green}</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Numbers and constants</string>
          <key>scope</key><string>constant.numeric, constant.language, constant.character</string>
          <key>settings</key><dict><key>foreground</key><string>${p.orange}</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Keywords and operators</string>
          <key>scope</key><string>keyword, storage, keyword.operator</string>
          <key>settings</key><dict><key>foreground</key><string>${p.pink}</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Functions</string>
          <key>scope</key><string>entity.name.function, support.function, meta.function-call</string>
          <key>settings</key><dict><key>foreground</key><string>${p.cyan}</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Types and classes</string>
          <key>scope</key><string>entity.name.type, entity.name.class, support.type, support.class</string>
          <key>settings</key><dict><key>foreground</key><string>${p.violet}</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Variables and properties</string>
          <key>scope</key><string>variable, variable.other.property, support.variable</string>
          <key>settings</key><dict><key>foreground</key><string>${p.ice}</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Tags and headings</string>
          <key>scope</key><string>entity.name.tag, markup.heading</string>
          <key>settings</key><dict><key>foreground</key><string>${p.pink}</string><key>fontStyle</key><string>bold</string></dict>
        </dict>
        <dict>
          <key>name</key><string>Invalid</string>
          <key>scope</key><string>invalid, invalid.illegal</string>
          <key>settings</key><dict><key>foreground</key><string>${p.foreground}</string><key>background</key><string>${p.red}</string></dict>
        </dict>
      </array>
    </dict>
    </plist>
  '';
in
{
  options.codex = {
    enable = mkEnableOption "Codex with dotfiles integrations";

    package = mkOption {
      type = types.package;
      default = pkgs.codex;
      defaultText = lib.literalExpression "pkgs.codex";
      description = "Underlying Codex CLI package.";
    };

    skills = mkOption {
      type = types.listOf types.str;
      default = availableSkills;
      description = ''
        Skills from the pinned skills input to link into Codex. Local-only
        skill directories that are not part of the input remain untouched.
      '';
    };

    manageConfig = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Let Home Manager own the complete Codex config.toml and import shared
        MCP servers. Leave disabled to preserve and narrowly update an
        existing mutable Codex configuration.
      '';
    };

    llmLog = {
      enable = mkEnableOption "routing Codex traffic through llm-log";
      baseUrl = mkOption {
        type = types.str;
        default = "http://127.0.0.1:8787";
        description = "Base URL of the local llm-log proxy.";
      };
    };
  };

  config = lib.mkMerge [
    {
      home.file = lib.listToAttrs (
        map (name: {
          name = ".codex/skills/${name}";
          value.source = "${skillsSource}/skills/${name}";
        }) cfg.skills
      );
    }

    (mkIf cfg.enable {
      assertions = [
        {
          assertion =
            cfg.manageConfig
            || (
              config.programs.codex.settings == { }
              && config.programs.codex.plugins == [ ]
              && config.programs.codex.marketplaces == { }
            );
          message = ''
            Set codex.manageConfig = true before configuring programs.codex.settings,
            programs.codex.plugins, or programs.codex.marketplaces. Those options make
            Home Manager own the complete Codex config.toml.
          '';
        }
      ];

      programs.codex = {
        enable = true;
        package = if cfg.llmLog.enable then wrappedPackage else cfg.package;
        enableMcpIntegration = cfg.manageConfig;
        settings = mkIf (cfg.manageConfig && outrun.enable) {
          tui.theme = outrun.name;
        };
      };

      home.file.".codex/themes/${outrun.name}.tmTheme" = mkIf outrun.enable {
        text = tmTheme;
      };

      home.activation.codexOutrunTheme = mkIf (outrun.enable && !cfg.manageConfig) (
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ${themeUpdater}/bin/apply-codex-config \
            ${lib.escapeShellArg "${codexHome}/config.toml"} \
            ${themePatch}
        ''
      );
    })
  ];
}
