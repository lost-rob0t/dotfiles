{ config, lib, pkgs, ... }:

let
  cfg = config.braveMcp;
  braveMcpPackage = pkgs.callPackage ../../packages/brave-mcp { };
in
{
  options.braveMcp = {
    enable = lib.mkEnableOption "Brave Search CLI MCP integration for OpenCode";
  };

  config = lib.mkIf cfg.enable {
    # Install bx as a first-class CLI as well as the thin stdio MCP adapter.
    home.packages = [
      pkgs.brave-search-cli
      braveMcpPackage
    ];

    # Personal Goggles re-ranking profiles:
    #   bx web "query" --goggles @~/.config/brave/goggles/<name>.goggle
    xdg.configFile."brave/goggles/daily-driver.goggle".source =
      ../files/goggles/daily-driver.goggle;
    xdg.configFile."brave/goggles/tech-research.goggle".source =
      ../files/goggles/tech-research.goggle;

    programs.mcp = {
      enable = true;
      servers.brave = {
        command = "${braveMcpPackage}/bin/brave-mcp";
        enabled = true;
      };
    };

    programs.opencode = {
      enable = lib.mkDefault true;
      enableMcpIntegration = true;
    };
  };
}
