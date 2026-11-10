{
  home-manager,
  lib,
  pkgs,
}:
let
  fixture = pkgs.runCommand "chatgpt-desktop" { } ''
        mkdir -p "$out/bin" "$out/share/applications"
        cat > "$out/bin/chatgpt" <<'SH'
    #!${pkgs.runtimeShell}
    exec "$CODEX_MCP_NODE_PATH" -e 'console.log(JSON.stringify({node:process.env.CODEX_MCP_NODE_PATH,args:process.argv.slice(1)}))' -- "$@"
    SH
        chmod +x "$out/bin/chatgpt"
        cat > "$out/share/applications/chatgpt.desktop" <<EOF
    [Desktop Entry]
    Type=Application
    Name=ChatGPT
    Exec=$out/bin/chatgpt %U
    Icon=chatgpt
    MimeType=x-scheme-handler/codex;
    EOF
  '';
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
  package = lib.findFirst (
    p: lib.getName p == "chatgpt-desktop-mcp-node"
  ) fixture home.config.home.packages;
in
pkgs.runCommand "chatgpt-desktop-launcher-check" { nativeBuildInputs = [ pkgs.jq ]; } ''
  export CODEX_MCP_NODE_PATH=/deliberately-invalid-node
  ${package}/bin/chatgpt 'codex://chat/a b' '--literal=$HOME' > direct.json
  jq -e --arg node '${lib.getExe pkgs.nodejs_24}' '
    .node == $node and .args == ["codex://chat/a b", "--literal=$HOME"]
  ' direct.json
  desktop='${package}/share/applications/chatgpt.desktop'
  launcher=$(sed -n 's/^Exec=\(.*\) %U$/\1/p' "$desktop")
  test "$launcher" = '${package}/bin/chatgpt'
  "$launcher" 'codex://chat/menu' > menu.json
  jq -e --arg node '${lib.getExe pkgs.nodejs_24}' '
    .node == $node and .args == ["codex://chat/menu"]
  ' menu.json
  grep -Fx 'MimeType=x-scheme-handler/codex;' "$desktop"
  grep -Fx 'Icon=chatgpt' "$desktop"
  touch "$out"
''
