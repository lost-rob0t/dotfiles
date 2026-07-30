{ config, lib, pkgs, ... }:

let
  cfg = config.proxmoxMcp;
  exampleConfig = ../../../scripts/proxmox-mcp.config.example.json;

  proxmoxMcpLauncher = pkgs.writeShellApplication {
    name = "proxmox-mcp-launcher";
    runtimeInputs = with pkgs; [
      coreutils
      jq
      uv
    ];
    text = ''
      config_file="''${PROXMOX_MCP_CONFIG:-${cfg.configFile}}"

      if [ ! -r "$config_file" ]; then
        echo "proxmox-mcp-launcher: configuration is missing or unreadable: $config_file" >&2
        echo "Run proxmox-mcp-init, then replace the placeholder host and API token." >&2
        exit 1
      fi

      if ! jq -e 'type == "object" and (.proxmox | type == "object") and (.auth | type == "object")' \
        "$config_file" >/dev/null; then
        echo "proxmox-mcp-launcher: invalid Proxmox MCP JSON: $config_file" >&2
        exit 1
      fi

      host="$(jq -r '.proxmox.host // empty' "$config_file")"
      user="$(jq -r '.auth.user // empty' "$config_file")"
      token_name="$(jq -r '.auth.token_name // empty' "$config_file")"
      token_value="$(jq -r '.auth.token_value // empty' "$config_file")"
      transport="$(jq -r '.mcp.transport // "STDIO"' "$config_file")"

      if [ -z "$host" ] || [ "$host" = "your-proxmox-host-ip" ]; then
        echo "proxmox-mcp-launcher: set proxmox.host in $config_file" >&2
        exit 1
      fi

      if [ -z "$user" ] || [ -z "$token_name" ]; then
        echo "proxmox-mcp-launcher: set auth.user and auth.token_name in $config_file" >&2
        exit 1
      fi

      case "$token_value" in
        "" | "your-token-value" | "replace-me")
          echo "proxmox-mcp-launcher: set auth.token_value in $config_file" >&2
          exit 1
          ;;
      esac

      if [ "$transport" != "STDIO" ]; then
        echo "proxmox-mcp-launcher: mcp.transport must be STDIO for Emacs mcp.el" >&2
        exit 1
      fi

      export PROXMOX_MCP_CONFIG="$config_file"
      exec uvx --from "proxmox-mcp-plus==${cfg.version}" proxmox-mcp-plus
    '';
  };

  proxmoxMcpInit = pkgs.writeShellApplication {
    name = "proxmox-mcp-init";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      config_file="${cfg.configFile}"
      config_dir="$(dirname "$config_file")"

      if [ -e "$config_file" ]; then
        echo "proxmox-mcp-init: refusing to overwrite $config_file" >&2
        exit 1
      fi

      install -d -m 0700 "$config_dir"
      install -m 0600 "${exampleConfig}" "$config_file"
      echo "Created $config_file"
      echo "Replace the placeholder Proxmox host, API token, and SSH settings before connecting."
    '';
  };
in
{
  options.proxmoxMcp = {
    enable = lib.mkEnableOption "Emacs-native Proxmox MCP integration";

    version = lib.mkOption {
      type = lib.types.str;
      default = "0.5.12";
      description = "Pinned proxmox-mcp-plus version executed by uvx.";
    };

    configFile = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.config/proxmox-mcp/config.json";
      description = "Secret Proxmox MCP JSON configuration kept outside the dotfiles repository.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      proxmoxMcpInit
      proxmoxMcpLauncher
    ];

    home.file.".config/proxmox-mcp/config.example.json".source = exampleConfig;
  };
}
