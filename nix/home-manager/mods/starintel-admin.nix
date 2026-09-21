{ config, lib, pkgs, ... }:
let
  cfg = config.starintelAdmin;
  adminRef = "github:starintel-labs/starintel-admin/90e3c226dc7013297b4e05781c79da16f321123a";
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
