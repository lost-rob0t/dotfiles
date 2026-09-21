{ config, lib, pkgs, ... }:
let
  cfg = config.starintelAdmin;
  adminRef = "github:starintel-labs/starintel-admin/7a3996e276ef39f145f7326d2c9e736f6cde47ba";
  launcher = pkgs.writeShellApplication {
    name = "starintel-admin";
    runtimeInputs = [ pkgs.nix ];
    text = ''
      exec nix --extra-experimental-features "nix-command flakes" run ${lib.escapeShellArg adminRef} -- "$@"
    '';
  };
in
{
  options.starintelAdmin = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install the pinned portable StarIntel administrator CLI launcher.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ launcher ];
  };
}
