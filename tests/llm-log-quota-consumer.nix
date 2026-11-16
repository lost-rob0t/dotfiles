# Evaluate the real module's service overlay without fetching/building packages.
# This covers its Nix expressions, not an entire Home Manager profile activation.
let
  evaluate = preferXdg: enabled:
    let
      module = import ../nix/home-manager/mods/llm.nix {
        lib = {
          mkIf = condition: value: if condition then value else { };
          mkAfter = value: value;
        };
        pkgs = { };
        inputs = { };
        config = {
          llm = {
            enable = true;
            quotaTelemetry = { enable = enabled; environmentFile = "/home/test/.config/llm-log/quotas.env"; };
          };
          home = { homeDirectory = "/home/test"; preferXdgDirectories = preferXdg; };
          xdg.configHome = "/home/test/.config";
          codex.package = "/nix/store/test-unwrapped-codex";
        };
      };
    in module.config.systemd.user.services.llm-log.Service;
  xdg = evaluate true true;
  legacy = evaluate false true;
  disabled = evaluate true false;
in
assert builtins.elem "LLM_LOG_QUOTAS_ENABLED=1" xdg.Environment;
assert builtins.elem "LLM_LOG_CODEX_BIN=/nix/store/test-unwrapped-codex/bin/codex" xdg.Environment;
assert builtins.elem "CODEX_HOME=/home/test/.config/codex" xdg.Environment;
assert builtins.elem "CODEX_HOME=/home/test/.codex" legacy.Environment;
assert xdg.EnvironmentFile == [ "-/home/test/.config/llm-log/quotas.env" ];
assert disabled == { };
{ enablement = true; unwrappedCodex = true; authDirectories = true; optionalPrivateEnvironment = true; disablement = true; }
