{ config, lib, pkgs, ... }:

let
  cfg = config.node-red;
in
{
  options.node-red = {
    enable = lib.mkEnableOption "Node-RED low-code programming environment";

    package = lib.mkPackageOption pkgs "node-red" { };

    userDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.node-red";
      description = "Node-RED userDir holding flows, settings and installed palettes.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 1880;
      description = "Editor/admin UI port.";
    };

    service.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Run Node-RED as a systemd user service.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.node-red = lib.mkIf cfg.service.enable {
      Unit = {
        Description = "Node-RED flow runtime";
        After = [ "network-online.target" ];
        WantedBy = [ "default.target" ];
      };

      Service = {
        ExecStart = "${lib.getExe' cfg.package "node-red"} --userDir ${lib.escapeShellArg cfg.userDir} --port ${toString cfg.port}";
        EnvironmentFile = "-${config.home.homeDirectory}/.config/node-red/env";
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
