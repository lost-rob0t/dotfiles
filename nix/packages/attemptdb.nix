{
  fetchurl,
  gzip,
  gnutar,
  lib,
  stdenvNoCC,
}:

let
  version = "0.2.9";
  assets = {
    "x86_64-linux" = {
      target = "x86_64-unknown-linux-musl";
      hash = "sha256-tGOCPVXtFVtDcVEnBXl8opsXqlOyauB2xgzHqUB/xbU=";
    };
    "aarch64-linux" = {
      target = "aarch64-unknown-linux-musl";
      hash = "sha256-BaK6cge5mcITooAeOloiTBRbDVnkj7S0DkW8x64WfUo=";
    };
    "x86_64-darwin" = {
      target = "x86_64-apple-darwin";
      hash = "sha256-Jec6W+9OjM91hw7JNpmV6gFRkL7z38zAeqTDrJjUhdw=";
    };
    "aarch64-darwin" = {
      target = "aarch64-apple-darwin";
      hash = "sha256-vUAsoMTbVZ6t0Netx+NLoKtUwQMNdlnXZWJAvYHpdik=";
    };
  };
  system = stdenvNoCC.hostPlatform.system;
  asset = assets.${system} or (throw "attemptdb: unsupported system ${system}");
  stem = "attempt-${version}-${asset.target}";
in
stdenvNoCC.mkDerivation {
  pname = "attemptdb";
  inherit version;

  src = fetchurl {
    url = "https://github.com/nullarch/attemptdb/releases/download/v${version}/${stem}.tar.gz";
    inherit (asset) hash;
  };

  dontUnpack = true;
  nativeBuildInputs = [
    gzip
    gnutar
  ];

  installPhase = ''
    runHook preInstall

    tar -xzf "$src"
    mkdir -p "$out/bin"
    install -m755 "${stem}/attempt" "$out/bin/attempt"
    install -m755 "${stem}/attempt-hook" "$out/bin/attempt-hook"

    runHook postInstall
  '';

  meta = {
    description = "Local-first coding-agent activity database used by VibeMon";
    homepage = "https://github.com/nullarch/attemptdb";
    license = lib.licenses.asl20;
    mainProgram = "attempt";
    platforms = builtins.attrNames assets;
  };
}
