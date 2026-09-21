{ config, lib, pkgs, ... }:
let
  cfg = config.starintelAdmin;
  adminRef = "github:starintel-labs/starintel-admin/8291ef189c1343f66c65a15d9b3db8f506f9a9d9";
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
