{ home-manager, inputs, lib, pkgs }:
let
  mkHome = extra: home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = { inherit inputs; };
    modules = [
      ../nix/home-manager/mods/outrun-theme.nix
      ../nix/home-manager/mods/codex.nix
      ../nix/home-manager/mods/cloudflare.nix
      {
        home = {
          username = "cloudflare-test";
          homeDirectory = "/home/cloudflare-test";
          stateVersion = "25.11";
        };
        codex = { enable = true; package = pkgs.hello; skills = [ ]; };
        programs.opencode.enable = true;
        cloudflare.enable = true;
      }
      extra
    ];
  };
  mutable = (mkHome { }).config;
  managed = (mkHome { codex.manageConfig = true; }).config;
  disabled = (mkHome { cloudflare.enable = lib.mkForce false; }).config;
  endpoint = "https://mcp.cloudflare.com/mcp";
  names = builtins.attrNames (builtins.readDir "${inputs.cloudflare-skills}/skills");
  python = pkgs.python3.withPackages (p: [ p.tomlkit ]);
in
assert mutable.programs.opencode.settings.mcp.cloudflare == {
  type = "remote";
  url = endpoint;
  enabled = true;
  oauth = { };
};
assert managed.home.file ? ".codex/config.toml";
assert !(mutable.home.file ? ".codex/config.toml");
assert mutable.home.activation ? cloudflareCodexMcp;
assert !(managed.home.activation ? cloudflareCodexMcp);
assert !(disabled.programs.mcp.servers ? cloudflare);
assert lib.all
  (name:
  mutable.home.file ? ".agents/skills/${name}"
  && mutable.home.file ? ".codex/skills/${name}"
    && mutable.home.file ? ".config/opencode/skills/${name}"
  )
  names;
pkgs.runCommand "cloudflare-agent-setup-check" { nativeBuildInputs = [ python ]; } ''
  cat > config.toml <<'TOML'
  # Preserve user settings and existing MCPs.
  model = "user-model"
  [mcp_servers.existing]
  command = "existing-server"
  TOML
  python3 ${../nix/home-manager/files/apply-codex-config.py} config.toml ${../nix/home-manager/files/cloudflare-codex-mcp.json}
  cp config.toml first.toml
  python3 ${../nix/home-manager/files/apply-codex-config.py} config.toml ${../nix/home-manager/files/cloudflare-codex-mcp.json}
  cmp config.toml first.toml
  python3 - <<'PY'
  import tomllib
  from pathlib import Path
  config = tomllib.loads(Path('config.toml').read_text())
  assert config['model'] == 'user-model'
  assert config['mcp_servers']['existing']['command'] == 'existing-server'
  assert config['mcp_servers']['cloudflare']['url'] == '${endpoint}'
  assert '# Preserve user settings' in Path('config.toml').read_text()
  PY
  python3 - <<'PYTHON'
  import tomllib
  from pathlib import Path
  config = tomllib.loads(Path('${managed.home.file.".codex/config.toml".source}').read_text())
  assert config['mcp_servers']['cloudflare']['url'] == '${endpoint}'
  PYTHON
  mkdir "$out"
  for skill in ${lib.escapeShellArgs names}; do
    test -s "${inputs.cloudflare-skills}/skills/$skill/SKILL.md"
  done
''
