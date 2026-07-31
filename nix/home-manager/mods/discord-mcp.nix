{ config, lib, pkgs, ... }:

let
  cfg = config.discordMcp;

  discordMcp = pkgs.buildNpmPackage {
    pname = "discord-mcp-server";
    version = "1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "v-3";
      repo = "discordmcp";
      rev = cfg.rev;
      hash = cfg.hash;
    };
    npmDepsHash = cfg.npmDepsHash;
  };

  discordMcpIndex =
    "${discordMcp}/lib/node_modules/discord-mcp-server/build/index.js";

  discordMcpLauncher = pkgs.writeShellApplication {
    name = "discord-mcp-launcher";
    runtimeInputs = [ pkgs.nodejs_22 ];
    text = ''
      if [ -z "''${DISCORD_TOKEN:-}" ]; then
        echo "discord-mcp-launcher: DISCORD_TOKEN is not set in the environment." >&2
        echo "The Emacs client injects it via mcp.el :env from auth-source." >&2
        exit 1
      fi
      exec node "${discordMcpIndex}" "$@"
    '';
  };
in
{
  options.discordMcp = {
    enable = lib.mkEnableOption "Emacs-native Discord MCP integration";

    rev = lib.mkOption {
      type = lib.types.str;
      default = "39622af0333bcd324e811a2390146927517fb03d";
      description = "Pinned upstream discordmcp revision to build.";
    };

    hash = lib.mkOption {
      type = lib.types.str;
      default = "sha256-zmWU9zDEJDvH0ywW990DgbDTdlxyQ8OIQ1okEUQNIuY=";
      description = "SRI hash of the upstream discordmcp source tarball.";
    };

    npmDepsHash = lib.mkOption {
      type = lib.types.str;
      default = "sha256-ONRO9smF8JSD1C5Xr4q3sET+83OaRqTbGxRbiryYg9A=";
      description = "SRI hash of the discordmcp npm dependency closure.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ discordMcpLauncher ];
  };
}
