{ config, lib, pkgs, ... }:

{
  nixpkgs.crossSystem.system = "armv7l-linux";
  imports = [
    <nixpkgs/nixos/modules/installer/sd-card/sd-image-aarch64.nix>
    ./rpi.nix
  ];
  # put your own configuration here, for example ssh keys:
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDAnKOESxRdse5Metw+vNVfL/6kmORgUYn2ejj2d7/5qwiFQUyxWTPk4rdFf1SWYstus4djeBfhr3r9Du8p03ypY+ZBKiFLhbh7YYsadnFK4+aMtqDaz4tZbrN+kDNUgeJO/sn7I9mSOYSUjwdJdmo8LkOzKrxa7Wqj/Whx6L+Vtx0EbF97zH7XBkngjX4qqv6I9thW6v4c8OwKLdjyYI+X3gCgCa/PHM4fToKsIk7QeHTQ6GuY1vV9ictmRUCwgHyQa15bTeJ59JBJBN8sVxfy0/z+4oPa9MFqyodU/pyJaHpg18YysP2lxrSnWkecbdCLIsBqkvsEkhuTdgTj65jYBXbehaK1++9ZM2p93RX0JnRhwSMCrjym2yaYhs0PVnUVkEcyNeyc1ifub++FCipr1XbJ+dU1cy+A2c/LdWma7ij6DJmaSWO8/jy5glsvjNmonDs15SVtdgD5L7HSlntgGMpcgbI3G1u2EpzV36ZUpLTqE0hlbO9efNEJG5jqARyjv8yOPJz9E1wdI5DLKpjbS4HXIof1DwjmR9VOiu6dnFvmhv7CSkvRGQxynYiw/W4beeVOp0dqnvvTODmpk7k3elSfS7BDNNGcyuG3qxlxKWp5E1FIn/S5KGzillVFjHQZyP0xoPBRjYwA9pNGJQUZXZBNEZa5LEWizeUH4tmYdw== unseen@flake"
  ];
}
