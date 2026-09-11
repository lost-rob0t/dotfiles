{ config, lib, pkgs, ... }:

let
  cfg = config.programs.starintelTunnel;

  starintelTunnel = pkgs.writeShellApplication {
    name = "starintel-tunnel";
    runtimeInputs = [ pkgs.openssh ];
    text = builtins.readFile ../../../scripts/starintel-tunnel;
  };
in
{
  options.programs.starintelTunnel.enable = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = "Install the StarIntel SSH tunnel helper and its tunnel map.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ starintelTunnel ];

    xdg.configFile."starintel/tunnels.conf".source =
      ../../../.config/starintel/tunnels.conf;
  };
}
