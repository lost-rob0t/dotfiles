{ pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/virtualisation/proxmox-lxc.nix")
    services.nix
  ];

  environment.systemPackages = [
    pkgs.vim
  ];
}
