{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.websearchMcp;
  # Meta web search (SearXNG + YaCy + Brave): `websearch` CLI + stdio MCP
  # server built from the public Forgejo repo (flake input, nixpkgs follows).
  websearchMcp = inputs.websearch-mcp.packages.${pkgs.stdenv.hostPlatform.system}.default;
in
{
  options.websearchMcp = {
    enable = lib.mkEnableOption "Meta web search MCP (SearXNG + YaCy + Brave) CLI and stdio server";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ websearchMcp ];

    # Provider credentials stay runtime/user state, never the Nix store:
    #   BRAVE_SEARCH_API_KEY / BRAVE_API_KEY_FILE (default ~/.config/brave/api_key)
    #   YACY_URL / YACY_USER / YACY_PASS (crawl control needs admin auth)
    #   SEARX_URL (default http://127.0.0.1:8080)
    programs.mcp = {
      enable = true;
      servers.websearch = {
        command = "${websearchMcp}/bin/websearch-mcp";
        args = [ "serve" ];
        enabled = true;
      };
    };

    programs.opencode = {
      enable = lib.mkDefault true;
      enableMcpIntegration = true;
    };
  };
}
