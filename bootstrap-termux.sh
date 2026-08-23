#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

readonly DOTFILES_REPO="https://github.com/lost-rob0t/dotfiles.git"
readonly DOTFILES_DIR="${STAR_DOTFILES_ROOT:-$HOME/.dotfiles}"
readonly DISTRO="${STAR_TERMUX_DISTRO:-debian}"
readonly DOOM_ROOT="${STAR_DOOM_ROOT:-$HOME/.config/emacs}"
readonly DOOMDIR_TERMUX="$DOTFILES_DIR/android/doom"

fail() {
  printf 'bootstrap-termux: %s\n' "$*" >&2
  exit 1
}

note() {
  printf '==> %s\n' "$*"
}

is_termux() {
  [[ -n "${PREFIX:-}" && "$PREFIX" == /data/data/com.termux/files/usr* ]] &&
    command -v pkg >/dev/null 2>&1
}

is_termux || fail "run this inside Termux"
command -v apt >/dev/null 2>&1 || fail "Termux apt is missing"

unset LD_LIBRARY_PATH

note "Enumerating host"
printf 'platform=termux\n'
printf 'arch=%s\n' "$(dpkg --print-architecture 2>/dev/null || uname -m)"
printf 'kernel=%s\n' "$(uname -srmo)"
printf 'android_release=%s\n' "$(getprop ro.build.version.release 2>/dev/null || true)"
printf 'android_sdk=%s\n' "$(getprop ro.build.version.sdk 2>/dev/null || true)"
printf 'device=%s %s\n' "$(getprop ro.product.manufacturer 2>/dev/null || true)" "$(getprop ro.product.model 2>/dev/null || true)"

note "Synchronizing Termux packages"
apt update
DEBIAN_FRONTEND=noninteractive apt full-upgrade -y

packages=(
  bash
  clang
  curl
  emacs
  fd
  gh
  git
  gnupg
  jq
  make
  nim
  openssh
  pkg-config
  proot-distro
  python
  ripgrep
  sqlite
  swi-prolog
  termux-api
)

case "$(dpkg --print-architecture 2>/dev/null || true)" in
  aarch64|x86_64|amd64)
    packages+=(sbcl)
    ;;
  *)
    printf '==> SBCL skipped: native Termux SBCL is 64-bit only on Android\n' >&2
    ;;
esac

note "Installing native Termux development stack"
pkg install -y "${packages[@]}"

if command -v termux-setup-storage >/dev/null 2>&1; then
  mkdir -p "$HOME/storage"
  termux-setup-storage >/dev/null 2>&1 || true
fi

note "Bootstrapping dotfiles at $DOTFILES_DIR"
if [[ -d "$DOTFILES_DIR/.git" ]]; then
  git -C "$DOTFILES_DIR" fetch origin master
  git -C "$DOTFILES_DIR" merge --ff-only origin/master
elif [[ -e "$DOTFILES_DIR" ]]; then
  fail "$DOTFILES_DIR exists but is not a Git checkout"
else
  git clone --branch master --single-branch "$DOTFILES_REPO" "$DOTFILES_DIR"
fi

note "Installing platform-aware entry command"
cat > "$PREFIX/bin/platform-enter" <<'PLATFORM_ENTER'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

readonly DISTRO="${STAR_TERMUX_DISTRO:-debian}"

host_type() {
  if [[ -n "${PREFIX:-}" && "$PREFIX" == /data/data/com.termux/files/usr* ]] && command -v pkg >/dev/null 2>&1; then
    printf 'termux\n'
  elif [[ -f /etc/NIXOS ]]; then
    printf 'nixos\n'
  elif grep -qi microsoft /proc/sys/kernel/osrelease 2>/dev/null; then
    printf 'wsl\n'
  elif [[ "$(uname -s 2>/dev/null || true)" == Linux ]]; then
    if [[ -r /etc/os-release ]]; then
      . /etc/os-release
      printf '%s\n' "${ID:-linux}"
    else
      printf 'linux\n'
    fi
  else
    uname -s 2>/dev/null | tr '[:upper:]' '[:lower:]'
  fi
}

print_info() {
  local host arch distro_id distro_version
  host="$(host_type)"
  arch="$(uname -m 2>/dev/null || true)"
  distro_id=''
  distro_version=''
  if [[ -r /etc/os-release ]]; then
    . /etc/os-release
    distro_id="${ID:-}"
    distro_version="${VERSION_ID:-}"
  fi
  printf 'host=%s\n' "$host"
  printf 'arch=%s\n' "$arch"
  printf 'kernel=%s\n' "$(uname -sr 2>/dev/null || true)"
  printf 'distro=%s\n' "$distro_id"
  printf 'distro_version=%s\n' "$distro_version"
  if [[ "$host" == termux ]]; then
    printf 'android_release=%s\n' "$(getprop ro.build.version.release 2>/dev/null || true)"
    printf 'android_sdk=%s\n' "$(getprop ro.build.version.sdk 2>/dev/null || true)"
    printf 'termux_prefix=%s\n' "${PREFIX:-}"
    printf 'guest=%s\n' "$DISTRO"
    if proot-distro login "$DISTRO" -- /bin/true >/dev/null 2>&1; then
      printf 'guest_ready=yes\n'
    else
      printf 'guest_ready=no\n'
    fi
  fi
}

if [[ "${1:-}" == --info ]]; then
  print_info
  exit 0
fi

host="$(host_type)"
if [[ "$host" != termux ]]; then
  if (( $# )); then
    exec "$@"
  fi
  exec "${SHELL:-/bin/sh}" -l
fi

command -v proot-distro >/dev/null 2>&1 || {
  printf 'platform-enter: proot-distro is not installed\n' >&2
  exit 127
}

if ! proot-distro login "$DISTRO" -- /bin/true >/dev/null 2>&1; then
  printf 'platform-enter: %s guest is not installed\n' "$DISTRO" >&2
  exit 1
fi

termux_home="${HOME:?}"
host_pwd="$(pwd -P)"
case "$host_pwd" in
  "$termux_home") guest_pwd='/root' ;;
  "$termux_home"/*) guest_pwd="/root/${host_pwd#"$termux_home"/}" ;;
  *) guest_pwd='/root' ;;
esac

if (( $# )); then
  exec proot-distro login "$DISTRO" --shared-home --work-dir "$guest_pwd" -- env STAR_PLATFORM_GUEST=1 "$@"
fi
exec proot-distro login "$DISTRO" --shared-home --work-dir "$guest_pwd" -- env STAR_PLATFORM_GUEST=1 /bin/bash -l
PLATFORM_ENTER
chmod 0755 "$PREFIX/bin/platform-enter"

note "Ensuring $DISTRO PRoot exists for glibc-only tools"
if ! proot-distro login "$DISTRO" -- /bin/true >/dev/null 2>&1; then
  proot-distro install "$DISTRO"
fi

note "Installing OpenCode in $DISTRO"
proot-distro login "$DISTRO" -- /bin/bash -lc '
  set -euo pipefail
  export DEBIAN_FRONTEND=noninteractive
  apt-get update
  apt-get install -y ca-certificates curl git tar
  installer="$(mktemp)"
  trap '\''rm -f "$installer"'\'' EXIT
  curl -fsSL https://opencode.ai/install -o "$installer"
  HOME=/root PATH=/usr/local/bin:/usr/bin:/bin bash "$installer" --no-modify-path
  install -m 0755 /root/.opencode/bin/opencode /usr/local/bin/opencode
  /usr/local/bin/opencode --version
'

note "Installing AI wrappers"
cat > "$PREFIX/bin/opencode" <<'OPENCODE_WRAPPER'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
exec platform-enter opencode "$@"
OPENCODE_WRAPPER
chmod 0755 "$PREFIX/bin/opencode"

cat > "$PREFIX/bin/ai" <<'AI_WRAPPER'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
readonly tool="${AI_TOOL:-opencode}"
exec platform-enter "$tool" "$@"
AI_WRAPPER
chmod 0755 "$PREFIX/bin/ai"

note "Installing Doom Emacs core"
if [[ -x "$DOOM_ROOT/bin/doom" ]]; then
  if [[ -d "$DOOM_ROOT/.git" ]]; then
    git -C "$DOOM_ROOT" pull --ff-only || true
  fi
elif [[ -e "$DOOM_ROOT" ]]; then
  fail "$DOOM_ROOT exists but is not a Doom Emacs checkout"
else
  git clone --depth 1 https://github.com/doomemacs/core "$DOOM_ROOT"
fi

[[ -f "$DOOMDIR_TERMUX/config.org" ]] || fail "missing $DOOMDIR_TERMUX/config.org"

note "Installing Termux Doom launchers"
cat > "$PREFIX/bin/doom-termux" <<EOF_DOOM
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
export DOOMDIR="$DOOMDIR_TERMUX"
exec "$DOOM_ROOT/bin/doom" "\$@"
EOF_DOOM
chmod 0755 "$PREFIX/bin/doom-termux"

cat > "$PREFIX/bin/emacs-termux" <<EOF_EMACS
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
export DOOMDIR="$DOOMDIR_TERMUX"
exec emacs --init-directory "$DOOM_ROOT" "\$@"
EOF_EMACS
chmod 0755 "$PREFIX/bin/emacs-termux"

cat > "$PREFIX/bin/emacsclient-termux" <<EOF_CLIENT
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
export DOOMDIR="$DOOMDIR_TERMUX"
if ! emacsclient -s termux-doom --eval t >/dev/null 2>&1; then
  emacs --init-directory "$DOOM_ROOT" --daemon=termux-doom >/dev/null 2>&1
fi
exec emacsclient -s termux-doom "\$@"
EOF_CLIENT
chmod 0755 "$PREFIX/bin/emacsclient-termux"

note "Synchronizing Termux Doom profile"
DOOMDIR="$DOOMDIR_TERMUX" "$DOOM_ROOT/bin/doom" sync
DOOMDIR="$DOOMDIR_TERMUX" "$DOOM_ROOT/bin/doom" sync --env || true

note "Installing Agent Zero tunnel helper"
install -m 0755 "$DOTFILES_DIR/scripts/agent-zero-tunnel" "$PREFIX/bin/agent-zero-tunnel"

note "Installing Termux:Widget shortcuts"
shortcuts="$HOME/.shortcuts"
tasks="$shortcuts/tasks"
mkdir -p "$shortcuts" "$tasks"
chmod 0700 "$shortcuts" "$tasks"

cat > "$shortcuts/01-StarIntel-Admin" <<'WIDGET_ADMIN'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
exec emacsclient-termux -t --eval '(star/termux-starintel-admin)'
WIDGET_ADMIN

cat > "$shortcuts/02-Org-Agenda" <<'WIDGET_AGENDA'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
exec emacsclient-termux -t --eval '(org-agenda nil "a")'
WIDGET_AGENDA

cat > "$shortcuts/03-TODO" <<'WIDGET_TODO'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
exec emacsclient-termux -t --eval '(org-todo-list)'
WIDGET_TODO

cat > "$shortcuts/04-AI" <<'WIDGET_AI'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
exec ai
WIDGET_AI

cat > "$shortcuts/05-Platform" <<'WIDGET_PLATFORM'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
platform-enter --info
printf '\nPress enter to close...'
read -r _
WIDGET_PLATFORM

cat > "$tasks/Sync-Dotfiles" <<'WIDGET_SYNC_DOTFILES'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
git -C "$HOME/.dotfiles" pull --ff-only
WIDGET_SYNC_DOTFILES

cat > "$tasks/Sync-Todos" <<'WIDGET_SYNC_TODOS'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail
if [[ -x "$HOME/.dotfiles/scripts/gpt-todos-sync" ]]; then
  exec "$HOME/.dotfiles/scripts/gpt-todos-sync"
fi
printf 'gpt-todos-sync is unavailable\n' >&2
exit 1
WIDGET_SYNC_TODOS

chmod 0700 "$shortcuts"/0* "$tasks"/*

if command -v am >/dev/null 2>&1 && pm list packages 2>/dev/null | grep -q '^package:com\.termux\.widget$'; then
  am broadcast -n com.termux.widget/.TermuxWidgetProvider \
    -a com.termux.widget.ACTION_REFRESH_WIDGET --ei appWidgetId 0 >/dev/null 2>&1 || true
else
  printf '==> Termux:Widget app not detected. Install the plugin from the same source/signing family as Termux, then add its widget.\n'
fi

note "Bootstrap complete"
printf 'Platform:  '
platform-enter --info | paste -sd ' ' -
printf '\nEmacs:     '
emacs --version | sed -n '1p'
printf 'Doom:      '
doom-termux version 2>/dev/null | sed -n '1p' || true
printf 'SWI:       '
swipl --version 2>/dev/null || true
printf 'SBCL:      '
sbcl --version 2>/dev/null || true
printf 'Nim:       '
nim --version 2>/dev/null | sed -n '1p' || true
printf 'Python:    '
python --version 2>/dev/null || true
printf 'OpenCode:  '
opencode --version 2>/dev/null || true
printf 'AI:        ai\n'
printf 'Doom UI:   emacs-termux\n'
printf 'Guest:     platform-enter\n'
