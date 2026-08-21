{ config, pkgs, ... }:
{
  imports = [
    ./base.nix
    ./desktop.nix
    ./fonts.nix
    ./emacs.nix
    ./security.nix
    ./nim.nix
    ./lisp.nix
    #./python.nix
    ./syncthing.nix
    ./media.nix
    ./pentesting.nix
    ./llm.nix
    ./proxmox-mcp.nix
    ./discord-mcp.nix
    ./unifi-mcp.nix
    ./home-manager-updater.nix
  ];
}
