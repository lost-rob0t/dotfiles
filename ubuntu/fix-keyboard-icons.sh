#!/usr/bin/env bash
set -Eeuo pipefail

readonly THEME_NAME='Outrun-Electric'
readonly SHELL_CSS="${XDG_DATA_HOME:-$HOME/.local/share}/themes/$THEME_NAME/gnome-shell/gnome-shell.css"

log() {
  printf '[ubuntu-outrun] %s\n' "$*"
}

have() {
  command -v "$1" >/dev/null 2>&1
}

schema_exists() {
  gsettings list-schemas | grep -Fxq "$1"
}

key_exists() {
  schema_exists "$1" && gsettings list-keys "$1" | grep -Fxq "$2"
}

set_setting() {
  local schema=$1 key=$2 value=$3
  if key_exists "$schema" "$key"; then
    gsettings set "$schema" "$key" "$value"
  fi
}

font_matches() {
  local candidate=$1 matched
  have fc-match || return 1
  matched=$(fc-match -f '%{family}\n' "$candidate" | head -n1)
  [[ ${matched,,} == *${candidate,,}* ]]
}

interface_font_family() {
  local candidate
  for candidate in 'Ubuntu Sans' 'Ubuntu' 'Cantarell' 'Noto Sans' 'Sans'; do
    if font_matches "$candidate"; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  printf '%s\n' 'Sans'
}

monospace_font_family() {
  local candidate
  for candidate in 'Hack Nerd Font' 'Hack Nerd Font Mono' 'Hack' 'Ubuntu Sans Mono' 'Ubuntu Mono' 'Monospace'; do
    if font_matches "$candidate"; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  printf '%s\n' 'Monospace'
}

first_complete_icon_theme() {
  local candidate root
  for candidate in Yaru Adwaita; do
    for root in /usr/share/icons "${XDG_DATA_HOME:-$HOME/.local/share}/icons" "$HOME/.icons"; do
      if [[ -d "$root/$candidate" ]]; then
        printf '%s\n' "$candidate"
        return
      fi
    done
  done
  printf '%s\n' 'Adwaita'
}

restore_interface_defaults() {
  local interface_font monospace_font icon_theme
  interface_font=$(interface_font_family)
  monospace_font=$(monospace_font_family)
  icon_theme=$(first_complete_icon_theme)

  set_setting org.gnome.desktop.interface font-name "'$interface_font 10'"
  set_setting org.gnome.desktop.interface document-font-name "'$interface_font 10'"
  set_setting org.gnome.desktop.interface monospace-font-name "'$monospace_font 11'"
  set_setting org.gnome.desktop.interface icon-theme "'$icon_theme'"
  set_setting org.gnome.desktop.wm.preferences titlebar-font "'$interface_font Bold 10'"

  log "interface font: $interface_font"
  log "monospace font: $monospace_font"
  log "icon theme: $icon_theme"
}

remove_panel_font_override() {
  [[ -f "$SHELL_CSS" ]] || return 0

  sed -i '/^[[:space:]]*font-family: "Hack Nerd Font", "Hack", monospace;[[:space:]]*$/d' "$SHELL_CSS"
  log "removed panel-wide Nerd Font override: $SHELL_CSS"
}

have gsettings || {
  log 'warning: gsettings is unavailable; only the generated Shell CSS will be patched'
  remove_panel_font_override
  exit 0
}

restore_interface_defaults
remove_panel_font_override
log 'log out and back in to reload GNOME Shell and keyboard/system icons'
