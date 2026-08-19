{ config, lib, pkgs, ... }:

let
  cfg = config.unifiMcp;
  exampleEnv = ../../../scripts/unifi-mcp.env.example;

  unifiMcpLauncher = pkgs.writeShellApplication {
    name = "unifi-mcp-launcher";
    runtimeInputs = with pkgs; [
      coreutils
      uv
    ];
    text = ''
      env_file="''${UNIFI_MCP_ENV:-${cfg.envFile}}"

      if [ ! -r "$env_file" ]; then
        echo "unifi-mcp-launcher: no readable config at $env_file" >&2
        echo "Run unifi-mcp-init, then configure the UDM Pro host and API key." >&2
        exit 1
      fi

      set -a
      # shellcheck disable=SC1090
      . "$env_file"
      set +a

      case "''${UNIFI_HOST:-}" in
        "" | "your-udmp-host")
          echo "unifi-mcp-launcher: set UNIFI_HOST in $env_file" >&2
          exit 1
          ;;
      esac

      case "''${UNIFI_API_KEY:-}" in
        "" | "replace-me")
          echo "unifi-mcp-launcher: set UNIFI_API_KEY in $env_file" >&2
          exit 1
          ;;
      esac

      export STUB_MODE=false
      export MCP_TRANSPORT=stdio
      export MCP_UNIFI_READONLY="''${MCP_UNIFI_READONLY:-${if cfg.readOnly then "true" else "false"}}"
      export MCP_UNIFI_MODULES_ENABLED="''${MCP_UNIFI_MODULES_ENABLED:-${cfg.modules}}"
      export MCP_UNIFI_AUDIT_PATH="''${MCP_UNIFI_AUDIT_PATH:-${cfg.auditPath}}"

      mkdir -p "$(dirname "$MCP_UNIFI_AUDIT_PATH")"

      exec uvx --from "git+https://github.com/pete-builds/mcp-unifi@${cfg.version}" mcp-unifi
    '';
  };

  unifiMcpInit = pkgs.writeShellApplication {
    name = "unifi-mcp-init";
    runtimeInputs = with pkgs; [ coreutils ];
    text = ''
      env_file="${cfg.envFile}"
      env_dir="$(dirname "$env_file")"

      if [ -e "$env_file" ]; then
        echo "unifi-mcp-init: refusing to overwrite $env_file" >&2
        exit 1
      fi

      install -d -m 0700 "$env_dir"
      install -m 0600 "${exampleEnv}" "$env_file"
      echo "Created $env_file"
      echo "Set UNIFI_HOST and UNIFI_API_KEY before connecting."
    '';
  };
in
{
  options.unifiMcp = {
    enable = lib.mkEnableOption "UDM Pro / UniFi MCP integration";

    version = lib.mkOption {
      type = lib.types.str;
      default = "v0.21.1";
      description = "Pinned pete-builds/mcp-unifi release executed by uvx.";
    };

    envFile = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.config/unifi-mcp/unifi-mcp.env";
      description = "Secret UniFi MCP environment file kept outside the dotfiles repository.";
    };

    readOnly = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Default MCP_UNIFI_READONLY policy when the env file does not override it.";
    };

    modules = lib.mkOption {
      type = lib.types.str;
      default = "network";
      description = "Default comma-separated UniFi MCP modules to enable.";
    };

    auditPath = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.stateHome}/unifi-mcp/audit.jsonl";
      description = "Default JSONL audit log path for UniFi MCP calls.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      unifiMcpInit
      unifiMcpLauncher
    ];

    home.file.".config/unifi-mcp/unifi-mcp.env.example".source = exampleEnv;
  };
}
