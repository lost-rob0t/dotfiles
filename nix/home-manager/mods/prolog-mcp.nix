{ config, lib, pkgs, ... }:

let
  cfg = config.prologMcp;

  prologMcpSource = builtins.fetchGit {
    url = "https://github.com/dicelab-rhul/PrologMCP.git";
    rev = cfg.revision;
  };

  skillSource = builtins.fetchGit {
    url = "https://github.com/lost-rob0t/skills.git";
    rev = cfg.skillRevision;
  };

  prologMcpPackage = pkgs.python3Packages.buildPythonApplication {
    pname = "prolog-mcp";
    version = "0.1.0";
    pyproject = true;
    src = prologMcpSource;

    build-system = [ pkgs.python3Packages.hatchling ];
    dependencies = [ pkgs.python3Packages.mcp ];

    # Upstream keeps harness.pl at the repository root, but its wheel only
    # contains src/prolog_mcp. Point the installed session manager at the
    # immutable pinned source tree rather than relying on a missing wheel file.
    postPatch = ''
      substituteInPlace src/prolog_mcp/session.py \
        --replace-fail \
          '_HARNESS_PL = _HERE.parent.parent / "harness.pl"' \
          '_HARNESS_PL = Path("${prologMcpSource}/harness.pl")'
    '';

    nativeBuildInputs = [ pkgs.makeWrapper ];
    nativeCheckInputs = with pkgs.python3Packages; [
      pytestCheckHook
      pytest-asyncio
      pkgs.swi-prolog
    ];

    preCheck = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"
      export PATH="${lib.makeBinPath [ pkgs.swi-prolog ]}:$PATH"
    '';

    # PrologMCP launches `swipl` subprocesses by name. Keep that dependency
    # explicit even when OpenCode is started from a minimal environment.
    postFixup = ''
      wrapProgram "$out/bin/prolog-mcp" \
        --prefix PATH : "${lib.makeBinPath [ pkgs.swi-prolog ]}"
    '';

    meta = {
      description = "MCP server exposing isolated SWI-Prolog sessions over stdio";
      homepage = "https://github.com/dicelab-rhul/PrologMCP";
      license = lib.licenses.mit;
      mainProgram = "prolog-mcp";
      platforms = lib.platforms.linux;
    };
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

    skillRevision = lib.mkOption {
      type = lib.types.str;
      default = "ecc1bbfd35039c0d3155e10efca6ad768b46807b";
      description = "Pinned lost-rob0t/skills revision containing the OpenCode Prolog skill.";
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
      skills.prolog-reasoning = "${skillSource}/opencode/prolog-reasoning";
    };
  };
}
