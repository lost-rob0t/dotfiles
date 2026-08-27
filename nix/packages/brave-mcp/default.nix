{
  lib,
  writeShellApplication,
  python3,
  brave-search-cli,
}:

writeShellApplication {
  name = "brave-mcp";

  runtimeInputs = [
    python3
    brave-search-cli
  ];

  text = ''
    export BRAVE_SEARCH_CLI_BIN="${brave-search-cli}/bin/bx"
    exec "${python3}/bin/python3" "${./server.py}" "$@"
  '';

  meta = {
    description = "Thin MCP stdio adapter over the Brave Search CLI";
    homepage = "https://github.com/brave/brave-search-cli";
    mainProgram = "brave-mcp";
    platforms = lib.platforms.linux;
  };
}
