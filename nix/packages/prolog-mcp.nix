{
  lib,
  makeWrapper,
  python3Packages,
  swi-prolog,
  revision ? "4ae536fd9b1cef8419d1798b5d4cb1569cba3cae",
}:

let
  source = builtins.fetchGit {
    url = "https://github.com/dicelab-rhul/PrologMCP.git";
    rev = revision;
  };
in
python3Packages.buildPythonApplication {
  pname = "prolog-mcp";
  version = "0.1.0";
  pyproject = true;
  src = source;

  build-system = [ python3Packages.hatchling ];
  dependencies = [ python3Packages.mcp ];

  # Upstream keeps harness.pl at the repository root, but its wheel only
  # contains src/prolog_mcp. Point the installed session manager at the
  # immutable pinned source tree rather than relying on a missing wheel file.
  postPatch = ''
    substituteInPlace src/prolog_mcp/session.py \
      --replace-fail \
        '_HARNESS_PL = _HERE.parent.parent / "harness.pl"' \
        '_HARNESS_PL = Path("${source}/harness.pl")'
  '';

  nativeBuildInputs = [ makeWrapper ];
  nativeCheckInputs = [
    python3Packages.pytestCheckHook
    python3Packages.pytest-asyncio
    swi-prolog
  ];

  preCheck = ''
    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    export PATH="${lib.makeBinPath [ swi-prolog ]}:$PATH"
  '';

  # PrologMCP launches `swipl` subprocesses by name. Keep that dependency
  # explicit even when OpenCode is started from a minimal environment.
  postFixup = ''
    wrapProgram "$out/bin/prolog-mcp" \
      --prefix PATH : "${lib.makeBinPath [ swi-prolog ]}"
  '';

  meta = {
    description = "MCP server exposing isolated SWI-Prolog sessions over stdio";
    homepage = "https://github.com/dicelab-rhul/PrologMCP";
    license = lib.licenses.mit;
    mainProgram = "prolog-mcp";
    platforms = lib.platforms.linux;
  };
}
