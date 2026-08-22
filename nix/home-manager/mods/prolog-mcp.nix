{ config, lib, pkgs, ... }:

let
  cfg = config.prologMcp;

  prologMcpPackage = pkgs.callPackage ../../packages/prolog-mcp.nix {
    revision = cfg.revision;
  };
in
{
  options.prologMcp = {
    enable = lib.mkEnableOption "local Prolog MCP integration for OpenCode";

    revision = lib.mkOption {
      type = lib.types.str;
      default = "4ae536fd9b1cef8419d1798b5d4cb1569cba3cae";
      description = "Pinned dicelab-rhul/PrologMCP Git revision.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ prologMcpPackage ];

    programs.mcp = {
      enable = true;
      servers.prolog = {
        command = "${prologMcpPackage}/bin/prolog-mcp";
        enabled = true;
      };
    };

    programs.opencode = {
      enable = lib.mkDefault true;
      enableMcpIntegration = true;
    };
  };
}
