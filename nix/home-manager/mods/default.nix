{ config, pkgs, inputs, ... }:
{
  imports = [
    inputs.skills.homeManagerModules.opencode
    inputs.skills.homeManagerModules.claude
    inputs.skills.homeManagerModules.agents
    ./base.nix
    ./desktop.nix
    ./gnome.nix
    ./fonts.nix
    ./emacs.nix
    ./gpt-todos.nix
    ./security.nix
    ./nim.nix
    ./lisp.nix
    #./python.nix
    ./syncthing.nix
    ./media.nix
    ./pentesting.nix
    ./llm.nix
    ./screen-capture.nix
    ./prolog-mcp.nix
    ./proxmox-mcp.nix
    ./discord-mcp.nix
    ./unifi-mcp.nix
    ./home-manager-updater.nix
  ];
}
