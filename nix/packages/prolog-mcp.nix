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

    # Upstream revision 4ae536f registers handlers with the decorator API
    # (@server.list_tools() etc.) that mcp>=2 removed in favor of
    # constructor injection. Port the registration, keeping the handler
    # bodies untouched.
    substituteInPlace src/prolog_mcp/server.py \
      --replace-fail 'server = Server("prolog-mcp")' 'server = None  # built in _serve(); see _build_server().' \
      --replace-fail '@server.list_tools()
async def list_tools() -> list[types.Tool]:' 'async def _list_tools() -> list[types.Tool]:' \
      --replace-fail '@server.call_tool()
async def call_tool(name: str, arguments: dict) -> list[types.TextContent]:' 'async def _call_tool(name: str, arguments: dict) -> list[types.TextContent]:' \
      --replace-fail '@server.list_prompts()
async def list_prompts() -> list[types.Prompt]:' 'async def _list_prompts() -> list[types.Prompt]:' \
      --replace-fail '@server.get_prompt()
async def get_prompt(name: str, arguments: dict[str, str] | None) -> types.GetPromptResult:' 'async def _get_prompt(name: str, arguments: dict[str, str] | None) -> types.GetPromptResult:' \
      --replace-fail '        init_opts = server.create_initialization_options()
        await server.run(read_stream, write_stream, init_opts)' '        srv = _build_server()
        init_opts = srv.create_initialization_options()
        await srv.run(read_stream, write_stream, init_opts)'

    cat >> src/prolog_mcp/server.py <<'EOF'


def _build_server() -> Server:
    """Wire the handlers above into an mcp>=2 Server via constructor injection."""

    async def on_list_tools(ctx, params):
        return types.ListToolsResult(tools=await _list_tools())

    async def on_call_tool(ctx, params):
        content = await _call_tool(params.name, dict(params.arguments or {}))
        return types.CallToolResult(content=content)

    async def on_list_prompts(ctx, params):
        return types.ListPromptsResult(prompts=await _list_prompts())

    async def on_get_prompt(ctx, params):
        return await _get_prompt(params.name, dict(params.arguments or {}))

    return Server(
        "prolog-mcp",
        on_list_tools=on_list_tools,
        on_call_tool=on_call_tool,
        on_list_prompts=on_list_prompts,
        on_get_prompt=on_get_prompt,
    )
EOF
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
