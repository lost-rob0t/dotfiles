{ lib, pkgs, inputs, config, ... }:

let
  cfg = config.zara;
  toml = pkgs.formats.toml { };

  discoveryFiles = lib.mapAttrs'
    (name: source:
      lib.nameValuePair ".zarathushtra/plugins/${name}" {
        inherit source;
      })
    cfg.plugins.discoveryFiles;

  pluginConfigFiles = lib.mapAttrs'
    (name: source:
      lib.nameValuePair ".config/zarathushtra/plugins/${name}" {
        inherit source;
      })
    cfg.plugins.configFiles;

  sensitiveConfigNames = lib.filter
    (name:
      let lower = lib.toLower name;
      in lib.hasInfix "token" lower
        || lib.hasInfix "secret" lower
        || lib.hasInfix "password" lower
        || lib.hasInfix "credential" lower)
    (lib.attrNames cfg.plugins.configFiles);
in
{
  options.zara = {
    enable = lib.mkEnableOption "Zara assistant binaries and services";

    package = lib.mkOption {
      type = lib.types.package;
      default = inputs.zara.packages.${pkgs.stdenv.hostPlatform.system}.zarathushtra;
      defaultText = lib.literalExpression "inputs.zara.packages.\${pkgs.stdenv.hostPlatform.system}.zarathushtra";
      description = ''
        Zara flake package providing the zara client, zara-server daemon and
        companion binaries.
      '';
    };

    nixManaged = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Take ownership of ~/.config/zarathushtra/config.toml from Nix. When
        false (default) this module never writes Zara configuration and any
        existing user configuration is left untouched. When true the file is
        generated from {option}`zara.settings` and replaced on every
        activation (Home Manager backs up an existing unmanaged file).
      '';
    };

    server = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Run zara-server as a Home Manager systemd user service.";
      };

      environmentFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "-%h/.config/zarathushtra/secrets.env";
        description = ''
          Optional systemd EnvironmentFile for zara-server. Keep this file
          outside the Nix store and outside Git; use it for values such as
          ZARA_DISCORD_TOKEN. Prefix the path with '-' to make a missing file
          non-fatal.
        '';
      };

      shutdownTimeout = lib.mkOption {
        type = lib.types.ints.u32;
        default = 240;
        description = ''
          Seconds passed to zara-server --shutdown-timeout. The daemon also
          spends this budget waiting for runtime startup before declaring the
          runtime degraded; the binary's 5 second default is far too short for
          cold AgentManager/ChromaDB starts, which can take minutes.
        '';
      };
    };

    desktop = {
      enable = lib.mkEnableOption "Zara desktop copilot systemd user service";
    };

    wake = {
      enable = lib.mkEnableOption "zara-wake wake-word listener user service";

      package = lib.mkOption {
        type = lib.types.package;
        default = inputs.zara.packages.${pkgs.stdenv.hostPlatform.system}.zara-wake;
        defaultText = lib.literalExpression "inputs.zara.packages.\${pkgs.stdenv.hostPlatform.system}.zara-wake";
        description = "Zara flake package providing the zara-wake listener binary.";
      };
    };

    plugins = {
      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
        description = ''
          Extra plugin packages installed into the Home Manager profile.
          Package selection belongs here; plugin discovery files are declared
          separately with {option}`zara.plugins.discoveryFiles`.
        '';
      };

      discoveryFiles = lib.mkOption {
        type = lib.types.attrsOf lib.types.path;
        default = { };
        example = lib.literalExpression ''
          {
            "starintel.py" = ../files/zarathushtra/plugins/starintel.py;
          }
        '';
        description = ''
          Declarative Zara discovery entries. Attribute names are filenames
          placed under ~/.zarathushtra/plugins and values are immutable source
          paths. Do not put secrets in these files.
        '';
      };

      configFiles = lib.mkOption {
        type = lib.types.attrsOf lib.types.path;
        default = { };
        description = ''
          Non-secret plugin runtime files placed below
          ~/.config/zarathushtra/plugins. This is intended for immutable code
          or assets such as a plugin library directory. Tokens, passwords,
          credentials and mutable policy databases must stay outside Nix.
        '';
      };
    };

    settings = lib.mkOption {
      type = toml.type;
      default = { };
      description = ''
        TOML settings serialized to ~/.config/zarathushtra/config.toml when
        {option}`zara.nixManaged` is enabled.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    warnings = lib.mkIf (cfg.nixManaged && cfg.settings == { }) [
      "zara.nixManaged is enabled while zara.settings is empty; activation will replace ~/.config/zarathushtra/config.toml with an empty configuration."
    ];

    assertions = [
      {
        assertion = sensitiveConfigNames == [ ];
        message = ''
          zara.plugins.configFiles must never contain secret-bearing paths.
          Refusing: ${lib.concatStringsSep ", " sensitiveConfigNames}. Use
          zara.server.environmentFile or another out-of-store secret source.
        '';
      }
    ];

    home.packages = [ cfg.package ] ++ cfg.plugins.packages;

    home.file = discoveryFiles // pluginConfigFiles // {
      ".config/zarathushtra/config.toml" = lib.mkIf cfg.nixManaged {
        source = toml.generate "zara-config.toml" cfg.settings;
      };
    };

    systemd.user.services.zara-server = lib.mkIf cfg.server.enable {
      Unit = {
        Description = "Long-lived Zara assistant daemon";
        After = [ "pipewire.service" "pipewire-pulse.service" ];
      };
      Service = {
        ExecStart = "${cfg.package}/bin/zara-server --shutdown-timeout ${toString cfg.server.shutdownTimeout}";
        EnvironmentFile = lib.optional (cfg.server.environmentFile != null) cfg.server.environmentFile;
        Restart = "on-failure";
        RestartSec = 5;
        UMask = "0077";
      };
      Install.WantedBy = [ "default.target" ];
    };

    systemd.user.services.zara-desktop = lib.mkIf cfg.desktop.enable {
      Unit = {
        Description = "Zara desktop copilot";
        After = [ "graphical-session.target" "zara-server.service" ];
        Wants = lib.optional cfg.server.enable "zara-server.service";
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${cfg.package}/bin/zara-desktop";
        Restart = "on-failure";
        RestartSec = 3;
        UMask = "0077";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };

    systemd.user.services.zara-wake = lib.mkIf cfg.wake.enable {
      Unit = {
        Description = "Zara wake-word voice listener";
        After = [ "graphical-session.target" "pipewire.service" "pipewire-pulse.service" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${cfg.wake.package}/bin/zara-wake";
        Restart = "on-failure";
        RestartSec = 5;
        UMask = "0077";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
