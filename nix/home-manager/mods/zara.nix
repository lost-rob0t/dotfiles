{ lib, pkgs, inputs, config, ... }:

let
  cfg = config.zara;
  toml = pkgs.formats.toml { };
in
{
  options.zara = {
    enable = lib.mkEnableOption "Zara assistant binaries and daemon";

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

    home.packages = [ cfg.package ];

    # Zara loads user tools from ~/.zarathushtra/plugins at startup.
    home.file.".zarathushtra/plugins/starintel.py".source =
      ../files/zarathushtra/plugins/starintel.py;

    home.file.".config/zarathushtra/config.toml" = lib.mkIf cfg.nixManaged {
      source = toml.generate "zara-config.toml" cfg.settings;
    };

    systemd.user.services.zara-server = {
      Unit = {
        Description = "Long-lived Zara assistant daemon";
        After = [ "pipewire.service" "pipewire-pulse.service" ];
      };
      Service = {
        ExecStart = "${cfg.package}/bin/zara-server";
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
