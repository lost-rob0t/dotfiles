{
  config,
  pkgs,
  ...
}:
let
  skill-sync = pkgs.writeShellApplication {
    name = "skill-sync";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.git
      pkgs.jq
      pkgs.nix
      config.programs.home-manager.package
    ];
    text = ''
      exec ${pkgs.bash}/bin/bash ${../../../scripts/skill-sync.sh} "$@"
    '';
  };
in
{
  home.packages = [ skill-sync ];
}
