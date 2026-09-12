{
  config,
  pkgs,
  ...
}:
let
  hourlyUpdater = pkgs.writeShellApplication {
    name = "starintel-hourly-update";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.git
      pkgs.openssh
      pkgs.util-linux
      config.programs.home-manager.package
    ];
    text = ''
      exec ${pkgs.bash}/bin/bash ${../../../scripts/starintel-hourly-update.sh} "$@"
    '';
  };

  installer = pkgs.writeShellApplication {
    name = "install-starintel-hourly-update";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.gnugrep
      pkgs.nix
    ];
    text = ''
      export STARINTEL_HOURLY_COMMAND="${config.home.profileDirectory}/bin/starintel-hourly-update"
      exec ${pkgs.bash}/bin/bash ${../../../scripts/install-starintel-hourly-update.sh} "$@"
    '';
  };
in
{
  home.packages = [
    hourlyUpdater
    installer
  ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    persistent = true;
    randomizedDelaySec = "30m";
  };
}
