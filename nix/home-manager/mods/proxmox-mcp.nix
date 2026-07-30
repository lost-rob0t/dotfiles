{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.proxmox-mcp;

  proxmoxEnvFile = "${config.home.homeDirectory}/.config/proxmox-mcp/proxmox-mcp.env";

  # Pinned source checkout provided by the flake input. This makes the
  # launcher reproducible without depending on a manual ~/git clone.
  proxmoxSrc = inputs.proxmox-mcp-plus.outPath;

  proxmox-mcp-launcher = pkgs.writeShellApplication {
    name = "proxmox-mcp-launcher";
    runtimeInputs = with pkgs; [
      uv
      coreutils
    ];
    text = ''
      # Source user-provided credentials if present. The env file lives
      # outside any git repo so real tokens never get committed.
      if [ -f "${proxmoxEnvFile}" ]; then
        # shellcheck disable=SC1090,SC1091
        . "${proxmoxEnvFile}"
      fi

      # Fail fast if required Proxmox credentials are missing so MCP clients
      # surface a readable error instead of a silent retry loop.
      if [ -z "''${PROXMOX_HOST:-}" ] || [ -z "''${PROXMOX_TOKEN_VALUE:-}" ]; then
        echo "proxmox-mcp-launcher: PROXMOX_HOST and PROXMOX_TOKEN_VALUE must be set." >&2
        echo "Populate ${proxmoxEnvFile} (see ~/.dotfiles/docs/proxmox-mcp.org)." >&2
        exit 1
      fi

      # Run the MCP server from the flake-pinned source checkout.
      exec uvx --from "${proxmoxSrc}" proxmox-mcp-plus
    '';
  };
in
{
  options = with lib; {
    proxmox-mcp = {
      enable = mkEnableOption "ProxmoxMCP-Plus MCP launcher and uv runtime";

      package = mkOption {
        type = types.package;
        default = proxmox-mcp-launcher;
        defaultText = literalExpression "proxmox-mcp-launcher";
        description = "Launcher package that sources credentials and runs the MCP server.";
      };
    };
  };

  config = with lib; mkIf cfg.enable {
    home.packages = [
      cfg.package
      pkgs.uv
    ];

    # Drop the credential env file template into the user's config so they
    # know the expected shape. Real values are filled in by the user.
    home.file.".config/proxmox-mcp/proxmox-mcp.env.example".source =
      ../../../scripts/proxmox-mcp.env.example;
  };
}
