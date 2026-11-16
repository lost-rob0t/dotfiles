{
  home-manager,
  lib,
  pkgs,
}:
let
  fixture = pkgs.stdenvNoCC.mkDerivation {
    pname = "chatgpt-desktop";
    version = "test";
    dontUnpack = true;
    installPhase = ''
            mkdir -p "$out/bin" "$out/share/applications" "$out/lib/chatgpt/resources/cua_node/bin"
            cat > "$out/bin/chatgpt" <<'SH'
      #!${pkgs.runtimeShell}
      resources="$(dirname "$0")/../lib/chatgpt/resources"
      exec "$resources/cua_node/bin/node" -e 'console.log(JSON.stringify({node:process.execPath,args:process.argv.slice(1)}))' -- "$@"
      SH
            chmod +x "$out/bin/chatgpt"
            printf '#!${pkgs.runtimeShell}\nexit 99\n' > "$out/lib/chatgpt/resources/cua_node/bin/node"
            chmod +x "$out/lib/chatgpt/resources/cua_node/bin/node"
            cat > "$out/share/applications/chatgpt.desktop" <<EOF
      [Desktop Entry]
      Type=Application
      Name=ChatGPT
      Exec=$out/bin/chatgpt %U
      Icon=chatgpt
      MimeType=x-scheme-handler/codex;
      EOF
    '';
  };
  home = home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs.inputs = { };
    modules = [
      ../nix/home-manager/mods/outrun-theme.nix
      ../nix/home-manager/mods/chatgpt-desktop.nix
      ({ lib, ... }: {
        options.codex.manageConfig = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };
        config = {
          home.username = "launcher-test";
          home.homeDirectory = "/home/launcher-test";
          home.stateVersion = "25.11";
          programs.chatgpt-desktop = {
            enable = true;
            package = fixture;
            appearance.enable = false;
          };
        };
      })
    ];
  };
  package = lib.findFirst (p: lib.getName p == "chatgpt-desktop") fixture home.config.home.packages;
in
pkgs.runCommand "chatgpt-desktop-launcher-check" { nativeBuildInputs = [ pkgs.jq ]; } ''
  export CODEX_MCP_NODE_PATH=/deliberately-invalid-node
  node=$(readlink -f '${lib.getExe pkgs.nodejs_24}')
  ${package}/bin/chatgpt 'codex://chat/a b' '--literal=$HOME' > direct.json
  jq -e --arg node "$node" '
    .node == $node and .args == ["codex://chat/a b", "--literal=$HOME"]
  ' direct.json
  desktop='${package}/share/applications/chatgpt.desktop'
  launcher=$(sed -n 's/^Exec=\(.*\) %U$/\1/p' "$desktop")
  test "$launcher" = '${package}/bin/chatgpt'
  "$launcher" 'codex://chat/menu' > menu.json
  jq -e --arg node "$node" '
    .node == $node and .args == ["codex://chat/menu"]
  ' menu.json
  test "$(readlink '${package}/lib/chatgpt/resources/cua_node/bin/node')" = '${lib.getExe pkgs.nodejs_24}'
  grep -Fx 'MimeType=x-scheme-handler/codex;' "$desktop"
  grep -Fx 'Icon=chatgpt' "$desktop"
  touch "$out"
''
