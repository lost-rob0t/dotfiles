#!/usr/bin/env bash
set -Eeuo pipefail

readonly BG='#170c32'
readonly BG_ALT='#202146'
readonly PURPLE='#92406e'
readonly ORANGE='#fba922'
readonly CYAN='#2de2e6'
readonly FG='#f3f4f5'
readonly MAGENTA='#f6019d'
readonly GREEN='#62ff00'
readonly RED='#dd546e'
readonly VIOLET='#9700cc'
readonly THEME_NAME='Outrun-Electric'
readonly STATE_ROOT="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles/ubuntu-outrun"
readonly THEME_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/themes/$THEME_NAME"
readonly WALLPAPER_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/backgrounds"
readonly WALLPAPER="$WALLPAPER_ROOT/outrun-electric.svg"

log() {
  printf '[ubuntu-outrun] %s\n' "$*"
}

fail() {
  printf '[ubuntu-outrun] error: %s\n' "$*" >&2
  exit 1
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

font_family() {
  local candidate
  for candidate in 'Hack Nerd Font' 'Hack Nerd Font Mono' 'Hack'; do
    if have fc-match && fc-match -f '%{family}\n' "$candidate" | head -n1 | grep -Fqi 'Hack'; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  printf '%s\n' 'Monospace'
}

first_installed_theme() {
  local candidate
  for candidate in "$@"; do
    if [[ -d "/usr/share/themes/$candidate" || -d "${XDG_DATA_HOME:-$HOME/.local/share}/themes/$candidate" || -d "$HOME/.themes/$candidate" ]]; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  printf '%s\n' 'Yaru-dark'
}

first_installed_icon_theme() {
  local candidate
  for candidate in "$@"; do
    if [[ -d "/usr/share/icons/$candidate" || -d "${XDG_DATA_HOME:-$HOME/.local/share}/icons/$candidate" || -d "$HOME/.icons/$candidate" ]]; then
      printf '%s\n' "$candidate"
      return
    fi
  done
  printf '%s\n' 'Yaru'
}

latest_backup() {
  find "$STATE_ROOT" -mindepth 1 -maxdepth 1 -type d -name 'backup-*' -printf '%T@ %p\n' 2>/dev/null \
    | sort -nr \
    | head -n1 \
    | cut -d' ' -f2-
}

backup() {
  local stamp backup_dir
  stamp=$(date +%Y%m%d-%H%M%S)
  backup_dir="$STATE_ROOT/backup-$stamp"
  mkdir -p "$backup_dir"

  dconf dump /org/gnome/ > "$backup_dir/gnome.dconf"

  for path in \
    "$HOME/.config/gtk-3.0/gtk.css" \
    "$HOME/.config/gtk-4.0/gtk.css"; do
    if [[ -f "$path" ]]; then
      mkdir -p "$backup_dir/$(dirname "${path#$HOME/}")"
      cp -a "$path" "$backup_dir/${path#$HOME/}"
    fi
  done

  if [[ -d "$THEME_ROOT" ]]; then
    mkdir -p "$backup_dir/theme"
    cp -a "$THEME_ROOT/." "$backup_dir/theme/"
  fi

  printf '%s\n' "$backup_dir" > "$STATE_ROOT/latest"
  log "backup: $backup_dir"
}

restore() {
  local backup_dir
  backup_dir=${1:-}
  if [[ -z "$backup_dir" && -f "$STATE_ROOT/latest" ]]; then
    backup_dir=$(<"$STATE_ROOT/latest")
  fi
  if [[ -z "$backup_dir" ]]; then
    backup_dir=$(latest_backup)
  fi
  [[ -n "$backup_dir" && -d "$backup_dir" ]] || fail 'no backup found'

  if [[ -f "$backup_dir/gnome.dconf" ]]; then
    dconf load /org/gnome/ < "$backup_dir/gnome.dconf"
  fi

  for relative in .config/gtk-3.0/gtk.css .config/gtk-4.0/gtk.css; do
    rm -f "$HOME/$relative"
    if [[ -f "$backup_dir/$relative" ]]; then
      mkdir -p "$HOME/$(dirname "$relative")"
      cp -a "$backup_dir/$relative" "$HOME/$relative"
    fi
  done

  rm -rf "$THEME_ROOT"
  if [[ -d "$backup_dir/theme" ]]; then
    mkdir -p "$THEME_ROOT"
    cp -a "$backup_dir/theme/." "$THEME_ROOT/"
  fi

  log 'restored GNOME, GTK, and Shell theme state'
  log 'log out and back in to reload GNOME Shell completely'
}

write_wallpaper() {
  mkdir -p "$WALLPAPER_ROOT"
  cat > "$WALLPAPER" <<EOF_WALLPAPER
<svg xmlns="http://www.w3.org/2000/svg" width="3840" height="2160" viewBox="0 0 3840 2160">
  <defs>
    <linearGradient id="sky" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="$BG"/>
      <stop offset="0.62" stop-color="$BG_ALT"/>
      <stop offset="1" stop-color="#090412"/>
    </linearGradient>
    <linearGradient id="sun" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="$ORANGE"/>
      <stop offset="1" stop-color="$MAGENTA"/>
    </linearGradient>
    <radialGradient id="glow">
      <stop offset="0" stop-color="$MAGENTA" stop-opacity="0.42"/>
      <stop offset="1" stop-color="$MAGENTA" stop-opacity="0"/>
    </radialGradient>
    <clipPath id="sun-clip"><circle cx="1920" cy="1180" r="390"/></clipPath>
  </defs>
  <rect width="3840" height="2160" fill="url(#sky)"/>
  <circle cx="1920" cy="1180" r="720" fill="url(#glow)"/>
  <circle cx="1920" cy="1180" r="390" fill="url(#sun)"/>
  <g clip-path="url(#sun-clip)" stroke="$BG" stroke-width="22">
    <path d="M1470 1000h900 M1470 1080h900 M1470 1170h900 M1470 1270h900 M1470 1380h900"/>
  </g>
  <path d="M0 1510L520 1170l470 250 390-330 390 300 450-410 470 390 460-300 710 440v650H0z" fill="#0b0619" stroke="$VIOLET" stroke-width="10"/>
  <g stroke="$CYAN" stroke-opacity="0.62" stroke-width="4" fill="none">
    <path d="M0 2160L1920 1470 3840 2160"/>
    <path d="M260 2160L1920 1470 3580 2160 M560 2160L1920 1470 3280 2160 M900 2160L1920 1470 2940 2160 M1260 2160L1920 1470 2580 2160 M1600 2160L1920 1470 2240 2160"/>
    <path d="M0 2075H3840 M0 1980H3840 M0 1870H3840 M0 1750H3840 M0 1620H3840 M0 1510H3840"/>
  </g>
  <text x="1920" y="340" text-anchor="middle" fill="$FG" font-family="Hack, monospace" font-size="82" letter-spacing="28">OUTRUN ELECTRIC</text>
  <text x="1920" y="440" text-anchor="middle" fill="$MAGENTA" font-family="Hack, monospace" font-size="34" letter-spacing="12">EMACS // QTILE // UBUNTU</text>
</svg>
EOF_WALLPAPER
}

write_gtk_css() {
  local target
  for target in "$HOME/.config/gtk-3.0/gtk.css" "$HOME/.config/gtk-4.0/gtk.css"; do
    mkdir -p "$(dirname "$target")"
    cat > "$target" <<EOF_GTK
/* Generated by ~/.dotfiles/ubuntu/apply-outrun.sh */
@define-color theme_bg_color $BG;
@define-color theme_fg_color $FG;
@define-color theme_base_color $BG_ALT;
@define-color theme_text_color $FG;
@define-color theme_selected_bg_color $MAGENTA;
@define-color theme_selected_fg_color #000000;
@define-color accent_bg_color $MAGENTA;
@define-color accent_fg_color #000000;
@define-color accent_color $CYAN;
@define-color destructive_bg_color $RED;
@define-color success_color $GREEN;
@define-color warning_color $ORANGE;

window,
.background,
dialog,
assistant {
  background-color: $BG;
  color: $FG;
}

headerbar,
.titlebar {
  background: $BG_ALT;
  color: $FG;
  border-bottom: 1px solid $PURPLE;
  box-shadow: none;
}

button,
entry,
textview,
.view,
.navigation-sidebar,
.sidebar,
popover contents,
menu,
.menu {
  background-color: $BG_ALT;
  color: $FG;
  border-color: $PURPLE;
}

button:hover,
row:hover,
item:hover {
  background-color: $PURPLE;
  color: $FG;
}

button:checked,
button:active,
selection,
*:selected {
  background-color: $MAGENTA;
  color: #000000;
}

entry:focus,
button:focus {
  border-color: $CYAN;
  box-shadow: inset 0 0 0 1px $CYAN;
}

link,
link:visited {
  color: $CYAN;
}

progressbar progress,
levelbar block.filled {
  background-color: $MAGENTA;
}

scrollbar slider {
  background-color: $PURPLE;
  border: 0;
  min-width: 8px;
  min-height: 8px;
}

scrollbar slider:hover {
  background-color: $MAGENTA;
}

tooltip {
  background-color: $BG_ALT;
  color: $FG;
  border: 1px solid $MAGENTA;
}
EOF_GTK
  done
}

write_shell_theme() {
  mkdir -p "$THEME_ROOT/gnome-shell"
  cat > "$THEME_ROOT/gnome-shell/gnome-shell.css" <<EOF_SHELL
/* Generated by ~/.dotfiles/ubuntu/apply-outrun.sh */
@import url("resource:///org/gnome/shell/theme/gnome-shell.css");

#panel {
  background-color: rgba(32, 33, 70, 0.92);
  border-bottom: 1px solid $PURPLE;
  color: $FG;
  font-family: "Hack Nerd Font", "Hack", monospace;
}

#panel .panel-button {
  color: $FG;
}

#panel .panel-button:hover,
#panel .panel-button:active,
#panel .panel-button:checked {
  background-color: $PURPLE;
  color: $FG;
}

.popup-menu-content,
.quick-settings,
.message-list,
.notification-banner,
.calendar,
.datemenu-calendar-column,
.osd-window,
.modal-dialog,
.switcher-list {
  background-color: rgba(23, 12, 50, 0.97);
  color: $FG;
  border: 1px solid $PURPLE;
  box-shadow: 0 0 18px rgba(246, 1, 157, 0.22);
}

.popup-menu-item:hover,
.popup-menu-item:focus,
.quick-toggle:checked,
.quick-menu-toggle .quick-toggle:checked,
.calendar-day-base:hover,
.calendar-day-base:focus,
.calendar-day-base:selected {
  background-color: $MAGENTA;
  color: #000000;
}

.search-entry,
StEntry {
  background-color: $BG_ALT;
  color: $FG;
  border: 1px solid $PURPLE;
  selection-background-color: $MAGENTA;
  selected-color: #000000;
}

.search-entry:focus,
StEntry:focus {
  border-color: $CYAN;
  box-shadow: inset 0 0 0 1px $CYAN;
}

.dash-background,
.workspace-thumbnail-indicator {
  background-color: rgba(32, 33, 70, 0.86);
  border-color: $MAGENTA;
}

.app-well-app-running-dot,
.page-indicator .page-indicator-icon {
  background-color: $CYAN;
}

StButton:hover,
StButton:focus {
  color: $FG;
  background-color: $PURPLE;
}
EOF_SHELL

  mkdir -p "$HOME/.themes"
  rm -rf "$HOME/.themes/$THEME_NAME"
  ln -s "$THEME_ROOT" "$HOME/.themes/$THEME_NAME"
}

apply_interface() {
  local gtk_theme icon_theme font
  gtk_theme=$(first_installed_theme 'Yaru-purple-dark' 'Yaru-dark' 'Adwaita-dark')
  icon_theme=$(first_installed_icon_theme 'Yaru-purple' 'Yaru' 'Adwaita')
  font=$(font_family)

  set_setting org.gnome.desktop.interface color-scheme "'prefer-dark'"
  set_setting org.gnome.desktop.interface gtk-theme "'$gtk_theme'"
  set_setting org.gnome.desktop.interface icon-theme "'$icon_theme'"
  set_setting org.gnome.desktop.interface cursor-theme "'Yaru'"
  set_setting org.gnome.desktop.interface font-name "'$font 10'"
  set_setting org.gnome.desktop.interface document-font-name "'$font 10'"
  set_setting org.gnome.desktop.interface monospace-font-name "'$font 11'"
  set_setting org.gnome.desktop.interface enable-animations true
  set_setting org.gnome.desktop.interface accent-color "'pink'"

  set_setting org.gnome.desktop.wm.preferences theme "'$gtk_theme'"
  set_setting org.gnome.desktop.wm.preferences titlebar-font "'$font Bold 10'"

  set_setting org.gnome.shell.extensions.dash-to-dock transparency-mode "'FIXED'"
  set_setting org.gnome.shell.extensions.dash-to-dock background-opacity 0.76
  set_setting org.gnome.shell.extensions.dash-to-dock dash-max-icon-size 36
  set_setting org.gnome.shell.extensions.dash-to-dock extend-height false
  set_setting org.gnome.shell.extensions.dash-to-dock custom-theme-shrink true
  set_setting org.gnome.shell.extensions.dash-to-dock show-trash false
  set_setting org.gnome.shell.extensions.dash-to-dock show-mounts false

  set_setting org.gnome.desktop.background color-shading-type "'solid'"
  set_setting org.gnome.desktop.background primary-color "'$BG'"
  set_setting org.gnome.desktop.background secondary-color "'$BG_ALT'"
  set_setting org.gnome.desktop.background picture-options "'zoom'"
  set_setting org.gnome.desktop.background picture-uri "'file://$WALLPAPER'"
  set_setting org.gnome.desktop.background picture-uri-dark "'file://$WALLPAPER'"
  set_setting org.gnome.desktop.screensaver picture-uri "'file://$WALLPAPER'"
  set_setting org.gnome.desktop.screensaver picture-options "'zoom'"

  if schema_exists org.gnome.shell.extensions.user-theme; then
    set_setting org.gnome.shell.extensions.user-theme name "'$THEME_NAME'"
  fi

  if have gnome-extensions; then
    gnome-extensions enable user-theme@gnome-shell-extensions.gcampax.github.com >/dev/null 2>&1 || true
  fi
}

apply_terminal() {
  local profile font palette
  schema_exists org.gnome.Terminal.ProfilesList || return 0

  profile=$(gsettings get org.gnome.Terminal.ProfilesList default | tr -d "'")
  [[ -n "$profile" ]] || return 0

  font=$(font_family)
  palette="['#170c32', '#dd546e', '#62ff00', '#fba922', '#92406e', '#f6019d', '#2de2e6', '#f3f4f5', '#202146', '#ff6b8a', '#8aff52', '#ffc766', '#b96ea0', '#ff4dba', '#72f7fa', '#ffffff']"

  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/use-theme-colors" false
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/background-color" "'$BG'"
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/foreground-color" "'$FG'"
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/bold-color" "'$MAGENTA'"
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/palette" "$palette"
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/use-system-font" false
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/font" "'$font 12'"
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/cursor-shape" "'block'"
  dconf write "/org/gnome/terminal/legacy/profiles:/:$profile/scrollbar-policy" "'never'"
}

main() {
  case "${1:-apply}" in
    apply)
      have gsettings || fail 'gsettings is required; run this inside an Ubuntu GNOME session'
      have dconf || fail 'dconf is required; install dconf-cli'
      [[ ${XDG_CURRENT_DESKTOP:-} == *GNOME* || ${DESKTOP_SESSION:-} == *ubuntu* ]] \
        || log 'warning: GNOME session not detected; files will still be installed'
      backup
      write_wallpaper
      write_gtk_css
      write_shell_theme
      apply_interface
      apply_terminal
      log "applied $THEME_NAME using the Qtile/Doom Outrun palette"
      log 'log out and back in to reload GNOME Shell and GTK applications'
      ;;
    restore)
      restore "${2:-}"
      ;;
    *)
      fail 'usage: bash ubuntu/apply-outrun.sh [apply|restore [backup-directory]]'
      ;;
  esac
}

main "$@"
