# logos

NixOS 26.05 workstation and installer configuration for `logos`.

## Build

```sh
nix flake check
nix build
nix build .#logos-iso
```

The installer ISO is written under `result/iso/`.

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

## Install

The `logos-iso` is a graphical LXQt live image. Boot it in UEFI mode, log in as
the live `nixos` user (autologin), connect to the network via NetworkManager,
identify the target disk with `lsblk`, then either double-click the **Install
Logos** launcher on the desktop or run from a terminal:

```sh
sudo install-logos /dev/nvme0n1
```

The wrapper is a thin layer over `disko-install` and owns argument parsing,
safety checks, and confirmation. It can install **any** `nixosConfigurations`
attr from the flake — not just `logos`. Run `sudo install-logos --help` for
the full reference:

```text
-s, --system ATTR       nixosConfigurations attr to install.
                          Default: logos. Examples: logos, desktop, laptop.
-l, --list-systems      Print every installable system attr in the flake.
-f, --flake PATH        Flake path (without #attr). Final URL is PATH#ATTR.
                          Default: /etc/logos on the ISO, or $LOGOS_FLAKE.
--disk NAME             disko disk name to overwrite (default: main).
-n, --dry-run           Evaluate and show the plan without writing.
--write-efi-boot-entries  Persist boot entry to host NVRAM (off by default).
--extra-args ARGS       Pass extra arguments verbatim to disko-install.
```

Install the default system (logos):

```sh
sudo install-logos /dev/nvme0n1
```

Install a different system, e.g. `desktop`:

```sh
sudo install-logos --system desktop /dev/nvme0n1
```

Discover what systems the flake exposes:

```sh
install-logos --list-systems
```

The command is destructive and requires typing `WIPE-LOGOS` before it modifies
the disk. The LUKS passphrase is requested interactively at install time.

After install completes, set a root password before rebooting:

```sh
nixos-enter --root /mnt -c 'passwd root'
```

### Disk layout

The layout is now declared natively in `disk.nix` via
[disko](https://github.com/nix-community/disko) instead of a hand-rolled bash
provisioner. disko produces the `fileSystems` and `boot.initrd.luks.devices`
entries directly, so buildability and installability share one source of truth:

- 1 GiB EFI system partition (`/boot`, vfat)
- one LUKS2 partition using all remaining space (mapper `cryptroot`)
- one Btrfs filesystem inside LUKS, label `logos`
- `@`, `@home`, `@nix`, `@log`, and `@snapshots` subvolumes

Root, home, and the Nix store share the same free-space pool instead of being
trapped in fixed-size partitions. Btrfs mount options
(`compress=zstd:3,discard=async,noatime,space_cache=v2`) and LUKS
`allowDiscards` are set on the disko declaration.

The active host retains Docker and enforces assertions against libvirt, Steam,
and Sunshine.
