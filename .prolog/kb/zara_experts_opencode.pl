% ZARA experts provided to OpenCode via the Prolog MCP server, plus the
% verified expert inventory (2026-11-14, HEAD b4df2ed).

%% Transport: prolog-mcp (dicelab-rhul/PrologMCP) as OpenCode MCP server
%% "prolog", declared by nix/home-manager/mods/prolog-mcp.nix and surfaced to
%% OpenCode through programs.opencode.enableMcpIntegration.
expert_transport(opencode, mcp_server(prolog)).
expert_query_recipe('read expert .zara/experts/<name>/kb/expert.pl text, MCP create_session(source_text=...), then run_goal/1 with UNQUALIFIED goals (the session wraps sources in its own module; qualified goals raise existence_error)').
expert_query_recipe('run_tests with test_source_text=<tests/expert-tests.pl contents> resolves repo-relative use_module against the real checkout and reports PlUnit results').
mcp_fix(root_cause, 'upstream pinned HEAD 4ae536fd registers handlers with removed mcp SDK decorator API (@server.list_tools etc.); mcp 2.2.0 lowlevel Server only accepts on_list_tools/on_call_tool/on_list_prompts/on_get_prompt constructor handlers').
mcp_fix(patch, 'nix/packages/prolog-mcp.nix postPatch substitutes server.py to constructor-handler API; upstream is dormant since 2026-06, so no upstream bump exists').
mcp_fix(verified, 'initialize/tools/list/create_session/run_goal/run_tests over stdio all green 2026-11-14; git expert PlUnit 5/5 through the server').

%% Verified working expert inventory (live swipl, 2026-11-14).
expert_verified(git).            expert_verified(home-manager).
expert_verified(dotfiles).       expert_verified(dotfiles_expert).
expert_verified(zara).           expert_verified(sysadmin).
expert_verified(javascript).     expert_verified(typescript).
expert_verified(java).           expert_verified(kotlin).
expert_verified(bash).           expert_verified(nix).
expert_verified(prolog).         expert_verified(python).
expert_verified(nim).            expert_verified(lisp).
expert_verified(common-lisp).    expert_verified(emacs-lisp).
expert_verified(style).          expert_verified(arch).
expert_verified(keyring).
expert_verified(emacs, suite(pytest_test_kb)).
expert_verified(emacs, suite(python_roundtrip)).
expert_verified(emacs, suite(plunit_corpus_tests), scope('only with generated corpus.pl from the zara-emacs-expert package; a bare swipl consult exits 0 while PlUnit never ran (false green)')).
expert_verified(language, indirect('consumed by nix/prolog/python/bash/javascript/typescript/java/nim experts whose suites pass')).

%% Long-term TODOs (no currently failing expert; these are coverage/deploy gaps).
long_term_todo(language_expert_suite, 'add dedicated tests for .zara/experts/language/kb/adapter_contract.pl (only exercised indirectly today)').
long_term_todo(default_nix_coverage, 'add arch, keyring, and language suites to .zara/experts/default.nix packaged-library test gate').
long_term_todo(install_expert_library, 'zara-expert-library flake package is in no profile; install it or document the MCP path as the only consumption route').
long_term_todo(emacs_corpus_test_false_green, 'emacs corpus-tests.pl exits 0 when corpus.pl is absent and PlUnit never runs; make the suite fail hard on missing generated corpus').
