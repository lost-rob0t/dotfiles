{ config, lib, pkgs, ... }:
let
  cfg = config.starintelAdmin;
  adminRef = "github:starintel-labs/starintel-admin/7a3996e276ef39f145f7326d2c9e736f6cde47ba";
  launcher = pkgs.writeShellApplication {
    name = "starintel-admin";
    runtimeInputs = [ pkgs.nix ];
    text = ''
      exec nix run ${lib.escapeShellArg adminRef} -- "$@"
    '';
  };
in
{
  options.starintelAdmin = {
    enable = lib.mkEnableOption "portable StarIntel administrator CLI";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ launcher ];
  };
}
