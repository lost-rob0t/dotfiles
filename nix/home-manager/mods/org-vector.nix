{ config, lib, pkgs, ... }:

let
  orgVectorRevision = "7dc56eec47be06a2df5bfe7dfe4ceb17167372a4";
  orgVectorFlake = builtins.getFlake "github:lost-rob0t/org-vector/${orgVectorRevision}";
  orgVectorPackage = orgVectorFlake.packages.${pkgs.stdenv.hostPlatform.system}.default;
in
{
  options.orgVector = {
    enable = lib.mkEnableOption "org-vector semantic search CLI for org-roam notes";
  };

  config = lib.mkIf config.orgVector.enable {
    home.packages = [ orgVectorPackage ];
  };
}
