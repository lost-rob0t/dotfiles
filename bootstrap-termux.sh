#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

readonly DOTFILES_REPO="https://github.com/lost-rob0t/dotfiles.git"
readonly DOTFILES_DIR="${STAR_DOTFILES_ROOT:-$HOME/.dotfiles}"
readonly DISTRO="debian"

fail() {
  printf 'bootstrap-termux: %s\n' "$*" >&2
  exit 1
}

command -v apt >/dev/null 2>&1 || fail "this script requires Termux apt"
command -v pkg >/dev/null 2>&1 || fail "this script must run inside Termux"

# Termux is rolling-release and does not support partial upgrades. Keep any
# caller-provided library search path from injecting incompatible libraries into
# package-manager subprocesses, then synchronize the whole prefix before adding
# bootstrap dependencies.
unset LD_LIBRARY_PATH

printf '==> Synchronizing Termux packages\n'
apt update
DEBIAN_FRONTEND=noninteractive apt full-upgrade -y

printf '==> Installing Termux packages\n'
pkg install -y \
  curl \
  emacs \
  fd \
  gh \
  git \
  gnupg \
  jq \
  openssh \
  proot-distro \
  ripgrep \
  sqlite

printf '==> Bootstrapping dotfiles at %s\n' "$DOTFILES_DIR"
if [[ -d "$DOTFILES_DIR/.git" ]]; then
  git -C "$DOTFILES_DIR" fetch origin master
  git -C "$DOTFILES_DIR" merge --ff-only origin/master
elif [[ -e "$DOTFILES_DIR" ]]; then
  fail "$DOTFILES_DIR exists but is not a Git checkout"
else
  git clone --branch master --single-branch "$DOTFILES_REPO" "$DOTFILES_DIR"
fi

printf '==> Installing Agent Zero tunnel helper\n'
install -m 0755 "$DOTFILES_DIR/scripts/agent-zero-tunnel" "$PREFIX/bin/agent-zero-tunnel"

if [[ ! -e "$HOME/.emacs" && ! -e "$HOME/.emacs.d" && ! -e "$HOME/.config/emacs" ]]; then
  printf '==> Activating the Android Emacs profile\n'
  ln -s "$DOTFILES_DIR/android" "$HOME/.emacs.d"
else
  printf '==> Existing Emacs configuration found; leaving it untouched\n'
fi

printf '==> Ensuring Debian proot is installed for OpenCode\n'
if ! proot-distro login "$DISTRO" -- /bin/true >/dev/null 2>&1; then
  proot-distro install "$DISTRO"
fi

printf '==> Installing OpenCode inside Debian\n'
proot-distro login "$DISTRO" -- /bin/bash -lc '
  set -euo pipefail
  export DEBIAN_FRONTEND=noninteractive
  apt-get update
  apt-get install -y ca-certificates curl git tar
  installer="$(mktemp)"
  trap '\''rm -f "$installer"'\'' EXIT
  curl -fsSL https://opencode.ai/install -o "$installer"
  HOME=/root PATH=/usr/bin:/bin bash "$installer" --no-modify-path
  install -m 0755 /root/.opencode/bin/opencode /usr/local/bin/opencode
  /usr/local/bin/opencode --version
'

printf '==> Installing the Termux OpenCode wrapper\n'
cat > "$PREFIX/bin/opencode" <<'EOF'
#!/data/data/com.termux/files/usr/bin/bash
set -euo pipefail

readonly DISTRO="debian"
readonly TERMUX_HOME="${HOME:?}"
readonly HOST_PWD="$(pwd -P)"

case "$HOST_PWD" in
  "$TERMUX_HOME")
    guest_pwd="/root"
    ;;
  "$TERMUX_HOME"/*)
    guest_pwd="/root/${HOST_PWD#"$TERMUX_HOME"/}"
    ;;
  *)
    guest_pwd="/root"
    ;;
esac

exec proot-distro login "$DISTRO" \
  --shared-home \
  --work-dir "$guest_pwd" \
  -- /usr/local/bin/opencode "$@"
EOF
chmod 0755 "$PREFIX/bin/opencode"

printf '\n==> Bootstrap complete\n'
printf 'Emacs:   '
emacs --version | sed -n '1p'
printf 'OpenCode: '
opencode --version
printf 'A0 tunnel: agent-zero-tunnel --help\n'

if gh auth status >/dev/null 2>&1; then
  printf 'GitHub:   authenticated\n'
else
  printf 'GitHub:   run: gh auth login && gh auth setup-git\n'
fi
