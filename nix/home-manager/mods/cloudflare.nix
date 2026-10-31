{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.cloudflare;
  patch = ../files/cloudflare-codex-mcp.json;
  endpoint = (builtins.fromJSON (builtins.readFile patch)).mcp_servers.cloudflare.url;
  skillsRoot = "${inputs.cloudflare-skills}/skills";
  skillNames = builtins.attrNames (lib.filterAttrs
    (name: kind: kind == "directory" && builtins.pathExists "${skillsRoot}/${name}/SKILL.md")
    (builtins.readDir skillsRoot));
  codexHome =
    if config.home.preferXdgDirectories then
      "${config.xdg.configHome}/codex" else "${config.home.homeDirectory}/.codex";
  python = pkgs.python3.withPackages (p: [ p.tomlkit ]);
in
{
  options.cloudflare.enable = lib.mkEnableOption "Cloudflare agent skills and OAuth MCP integration";

  config = lib.mkIf cfg.enable {
    home.file = lib.listToAttrs (lib.concatMap
      (name: map
        (directory: {
          name = "${directory}/${name}";
          value.source = "${skillsRoot}/${name}";
        }) [ ".agents/skills" ".codex/skills" ".config/opencode/skills" ])
      skillNames);

    programs.mcp = {
      enable = true;
      servers.cloudflare.url = endpoint;
    };
    programs.opencode = lib.mkIf config.programs.opencode.enable {
      enableMcpIntegration = true;
      settings.mcp.cloudflare = {
        type = "remote";
        url = endpoint;
        enabled = true;
        oauth = { };
      };
    };

    home.activation.cloudflareCodexMcp = lib.mkIf (config.codex.enable && !config.codex.manageConfig) (
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD ${python}/bin/python3 ${../files/apply-codex-config.py} \
          ${lib.escapeShellArg "${codexHome}/config.toml"} ${patch}
      ''
    );
  };
}
