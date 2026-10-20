{
  homeConfigurations,
  lib,
  pkgs,
}:

let
  flakeHome = homeConfigurations."unseen@flake";
  desktopHome = homeConfigurations."unseen@desktop";
  flakeCommand = flakeHome.config.systemd.user.services.zara-server.Service.ExecStart;
  desktopCommand = desktopHome.config.systemd.user.services.zara-server.Service.ExecStart;
in
pkgs.runCommand "zara-server-home-manager-check" { nativeBuildInputs = [ pkgs.gnugrep ]; } ''
  printf '%s\n' ${lib.escapeShellArg flakeCommand} | grep -Fq -- '--remote-endpoint tcp://0.0.0.0:6060'
  printf '%s\n' ${lib.escapeShellArg flakeCommand} | grep -Fq -- '--security-dir /home/unseen/.local/state/zarathushtra/security'
  if printf '%s\n' ${lib.escapeShellArg flakeCommand} | grep -Fq -- ' --endpoint '; then
    echo 'local IPC must remain the primary endpoint' >&2
    exit 1
  fi
  if printf '%s\n' ${lib.escapeShellArg desktopCommand} | grep -Fq -- '--remote-endpoint'; then
    echo 'shared desktop profile must not bind the flake host address' >&2
    exit 1
  fi
  ${flakeHome.config.zara.package}/bin/zara-server --help | grep -Fq -- '--remote-endpoint'
  touch "$out"
''
