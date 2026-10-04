{
  lib,
  writeShellApplication,
  bash,
  coreutils,
  curl,
  jq,
  openssl,
}:

writeShellApplication {
  name = "starintel-admin";
  runtimeInputs = [
    bash
    coreutils
    curl
    jq
    openssl
  ];
  text = builtins.readFile ./bin/starintel-admin;

  meta = {
    description = "Portable StarIntel operator administration CLI";
    homepage = "https://github.com/starintel-labs/starintel-admin";
    mainProgram = "starintel-admin";
    platforms = lib.platforms.linux;
  };
}
