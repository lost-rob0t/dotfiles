#!/usr/bin/env bash
set -Eeuo pipefail

readonly START='/* BEGIN OUTRUN TRANSPARENT SIDEBAR */'
readonly END='/* END OUTRUN TRANSPARENT SIDEBAR */'

log() {
  printf '[ubuntu-outrun] %s\n' "$*"
}

rewrite_css() {
  local target=$1 temporary
  mkdir -p "$(dirname "$target")"
  touch "$target"

  temporary=$(mktemp)
  awk -v start="$START" -v end="$END" '
    $0 == start { skipping = 1; next }
    $0 == end   { skipping = 0; next }
    !skipping   { print }
  ' "$target" > "$temporary"

  cat >> "$temporary" <<'EOF_CSS'

/* BEGIN OUTRUN TRANSPARENT SIDEBAR */
.navigation-sidebar,
.navigation-sidebar > viewport,
.navigation-sidebar > viewport > list,
.navigation-sidebar list,
.sidebar,
.sidebar-pane,
.sidebar-pane > scrolledwindow,
.sidebar-pane > scrolledwindow > viewport,
.sidebar-pane list,
navigation-split-view > sidebar,
navigation-split-view > sidebar > box,
preferenceswindow .navigation-sidebar,
preferenceswindow .sidebar,
windowhandle .sidebar,
.sidebar headerbar,
headerbar.sidebar {
  background-color: transparent;
  background-image: none;
  box-shadow: none;
  border-color: transparent;
}

.navigation-sidebar row:not(:selected),
.sidebar row:not(:selected) {
  background-color: transparent;
  background-image: none;
}
/* END OUTRUN TRANSPARENT SIDEBAR */
EOF_CSS

  mv "$temporary" "$target"
  log "transparent sidebar CSS: $target"
}

rewrite_css "$HOME/.config/gtk-3.0/gtk.css"
rewrite_css "$HOME/.config/gtk-4.0/gtk.css"

log 'close and reopen Settings; log out if libadwaita keeps the old stylesheet cached'
