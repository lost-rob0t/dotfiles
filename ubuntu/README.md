# Ubuntu Outrun desktop

This applies the same visual language used by Doom Emacs (`doom-outrun-electric`) and Qtile:

- background: `#170c32`
- raised surfaces: `#202146`
- border purple: `#92406e`
- primary accent: `#f6019d`
- secondary accent: `#2de2e6`
- foreground: `#f3f4f5`
- warning: `#fba922`
- success: `#62ff00`
- error: `#dd546e`

## Apply

Run from the dotfiles checkout while logged into Ubuntu GNOME:

```bash
bash ~/.dotfiles/ubuntu/apply.sh apply
```

The wrapper applies the main theme, restores GNOME-compatible keyboard/system icon rendering, and removes sidebar fills so Settings and other libadwaita applications inherit the window background. Selected sidebar rows keep the neon accent.

The script changes the current user's GNOME/Ubuntu settings only. It creates a timestamped backup under:

```text
~/.local/state/dotfiles/ubuntu-outrun/
```

It configures:

- GTK 3 and GTK 4 application colors
- transparent application sidebars with accented active rows
- GNOME Shell panels, menus, quick settings, notifications, overview, and dash
- Yaru dark base theme with pink/purple accent selection where supported
- Ubuntu Sans/Cantarell for GNOME interface text and Hack/Hack Nerd Font for monospace text when installed
- a complete Yaru or Adwaita icon theme for keyboard and system glyph coverage
- GNOME Terminal's full 16-color palette
- a generated 4K Outrun wallpaper and lock-screen background
- a compact translucent Ubuntu Dock

For full GNOME Shell styling, install the Ubuntu extension package once:

```bash
sudo apt install gnome-shell-extensions
```

Then log out and back in after applying the theme.

## Patch an existing installation

When the main theme is already applied, repair missing keyboard/system icons without rebuilding the theme:

```bash
bash ~/.dotfiles/ubuntu/fix-keyboard-icons.sh
```

Update only the sidebar CSS with:

```bash
bash ~/.dotfiles/ubuntu/fix-transparent-sidebar.sh
```

Close and reopen Settings afterward. Log out and back in after the icon repair so GNOME Shell reloads its stylesheet and font choices.

## Restore

Restore the most recent backup:

```bash
bash ~/.dotfiles/ubuntu/apply.sh restore
```

Restore a specific backup:

```bash
bash ~/.dotfiles/ubuntu/apply.sh restore ~/.local/state/dotfiles/ubuntu-outrun/backup-YYYYMMDD-HHMMSS
```
