% Verified by checks.x86_64-linux.chatgpt-desktop-launcher.
owner(chatgpt_mcp_node, 'nix/home-manager/mods/chatgpt-desktop.nix').
invariant(chatgpt_mcp_node, scoped_store_pinned_nodejs_24).
invariant(chatgpt_desktop_entry, launches_same_wrapped_binary_as_shell).
failure_mode(bundled_cua_node, elf_loader_segfault_before_mcp_start).
repair_recipe(bundled_cua_node, desktop_launcher_codex_mcp_node_path_override).
