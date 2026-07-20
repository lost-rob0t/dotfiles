# logos

NixOS 26.05 workstation and installer configuration for `logos`.

## Build

```sh
nix flake check
nix build .#logos
nix build .#logos-iso
nix run .#install-logos -- --help
```

The graphical installer ISO is written under `result/iso/`.

## Installer interface

The ISO boots into an LXQt live desktop and automatically logs in as `nixos`.
Open **Install Logos** from the desktop to select a disk, choose the flake,
set the `unseen` user password, and start the installation.

The same installer is available without a GUI:

```sh
install-logos --help
sudo install-logos --disk /dev/nvme0n1
```

It can also run directly from a flake checkout:

```sh
sudo nix run .#install-logos -- --disk /dev/nvme0n1
```

To install another compatible Logos flake configuration:

```sh
sudo install-logos \
  --flake /path/to/flake#logos \
  --disk /dev/nvme0n1
```

Preview evaluation without modifying a disk:

```sh
install-logos --dry-run --disk /dev/nvme0n1
```

## Disk layout

The installer uses Disko for one declarative format, mount, and NixOS install
operation. The flake owns the disk layout instead of relying on a separate shell
provisioning pass.

The destructive format mode creates:

- 1 GiB EFI system partition
- one LUKS2 partition using all remaining space
- one Btrfs filesystem inside LUKS
- `@`, `@home`, `@nix`, `@log`, and `@snapshots` subvolumes

Root, home, and the Nix store share one free-space pool instead of fixed-size
partitions.

## Publish an installer release

After this configuration is merged, run the release helper from anywhere inside
the repository:

```sh
./scripts/tag-logos-release.sh
```

It fetches remote tags, requires a clean working tree, and creates the next
available date-based tag:

```text
logos-2026.07.17
logos-2026.07.17.2
logos-2026.07.17.3
```

Use `--dry-run` to preview the next tag or `--yes` to skip confirmation:

```sh
./scripts/tag-logos-release.sh --dry-run
./scripts/tag-logos-release.sh --yes
```

Pushing the tag starts the **Publish Logos ISO** workflow, which creates or
updates the matching GitHub Release. The release contains directly downloadable
assets, not an Actions ZIP:

- `logos-nixos-2026.07.17.iso`
- `logos-nixos-2026.07.17.iso.sha256`

The workflow can also be run manually from GitHub Actions with a `logos-*`
release tag.

The active host retains Docker and enforces assertions against libvirt, Steam,
and Sunshine.
