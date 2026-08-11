# Logos Home Manager bootstrap

This repository exposes `logos` as a NixOS host and `unseen@logos` as the standalone Home Manager configuration.

## Existing Nix install

If Nix is already installed on a non-NixOS machine, clone the repository and activate the Home Manager profile directly through the flake:

```bash
git clone git@github.com:lost-rob0t/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
nix run github:nix-community/home-manager/release-26.05 -- switch --flake '.#unseen@logos'
```

A global `home-manager` command is not required for the first activation.

## Username

The Logos configuration targets the normal `unseen` account:

```nix
home.username = "unseen";
home.homeDirectory = "/home/unseen";
```

Confirm the local account before activation:

```bash
whoami
printf '%s\n' "$HOME"
```

Expected output is `unseen` and `/home/unseen`.

## Inspect available outputs

```bash
cd ~/.dotfiles
nix flake show
```

Relevant outputs include:

- `nixosConfigurations.logos`
- `homeConfigurations."unseen@logos"`
- `homeConfigurations."unseen@desktop"`
- `homeConfigurations."unseen@hunter02"`

For a normal Ubuntu laptop with Nix already installed, prefer the standalone Home Manager output rather than the NixOS installer.
