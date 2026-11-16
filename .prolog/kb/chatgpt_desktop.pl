% Verified by checks.x86_64-linux.chatgpt-desktop-launcher.
owner(chatgpt_mcp_node, 'nix/home-manager/mods/chatgpt-desktop.nix').
invariant(chatgpt_mcp_node, bundled_path_links_to_store_pinned_nodejs_24).
failure_mode(bundled_cua_node, elf_loader_segfault_before_mcp_start).
failure_mode(inherited_codex_mcp_node_path, overwritten_by_desktop_runtime_selection).
repair_recipe(bundled_cua_node, replace_standard_node_executable_during_nix_build).
