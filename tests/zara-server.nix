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
  zaraAuthPackage = lib.findFirst
    (package: lib.getName package == "zara-auth")
    null
    flakeHome.config.home.packages;
  testClientKey = "-.6(Vfv?M9JT!Y^l)iow0lAtnD(*w[0(d)(@EuI1";
in
assert zaraAuthPackage != null;
pkgs.runCommand "zara-server-home-manager-check" {
  nativeBuildInputs = [
    pkgs.gnugrep
    pkgs.jq
  ];
} ''
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

  auth_dir="$TMPDIR/zara-auth"
  auth_bin="${zaraAuthPackage}/bin/zara-auth"
  "$auth_bin" --help | grep -Fq -- 'enroll DEVICE_ID Z85_KEY'
  server_key="$("$auth_bin" --security-dir "$auth_dir" init)"
  test -n "$server_key"
  test "$server_key" = "$("$auth_bin" --security-dir "$auth_dir" public-key)"

  enrolled="$("$auth_bin" --security-dir "$auth_dir" enroll android ${lib.escapeShellArg testClientKey})"
  printf '%s\n' "$enrolled" | jq -e '.device_id == "android" and .active == true' >/dev/null
  "$auth_bin" --security-dir "$auth_dir" list --json |
    jq -e 'length == 1 and .[0].device_id == "android" and .[0].active == true' >/dev/null

  status="$("$auth_bin" --security-dir "$auth_dir" status)"
  printf '%s\n' "$status" | grep -Fq -- 'initialized=true'
  printf '%s\n' "$status" | grep -Fq -- 'clients_total=1'
  printf '%s\n' "$status" | grep -Fq -- 'clients_active=1'

  revoked="$("$auth_bin" --security-dir "$auth_dir" revoke android)"
  printf '%s\n' "$revoked" | jq -e '.device_id == "android" and .active == false' >/dev/null
  "$auth_bin" --security-dir "$auth_dir" list --json |
    jq -e 'length == 1 and .[0].device_id == "android" and .[0].active == false' >/dev/null

  touch "$out"
''
