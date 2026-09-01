{ config, lib, pkgs, inputs, ... }:

let
  quasarPin = "80553699fa6c9dec227d4ddff629c3ab3a8b8010";
  quasarSource = builtins.fetchGit {
    url = "https://github.com/lost-rob0t/quasar.git";
    rev = quasarPin;
  };

  quasarRuntimeLibs = with pkgs; [
    openssl
    rabbitmq-c
    libffi
    sqlite
    lmdb
  ];

  quasarStart = pkgs.writeShellApplication {
    name = "quasar-start";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      curl
      git
      nodejs_22
      sbcl
      pkg-config
      chromium
      gcc
      gnumake
    ] ++ quasarRuntimeLibs;
    text = ''
      pin='${quasarPin}'
      cache_root="''${XDG_CACHE_HOME:-$HOME/.cache}/quasar-pinned"
      workdir="$cache_root/$pin"
      source_path='${quasarSource}'

      if curl --fail --silent --max-time 1 http://127.0.0.1:5173/ >/dev/null 2>&1; then
        echo "Quasar is already running at http://127.0.0.1:5173"
        exit 0
      fi

      mkdir -p "$cache_root"
      if [ ! -f "$workdir/.quasar-pin" ] || [ "$(cat "$workdir/.quasar-pin" 2>/dev/null || true)" != "$pin" ]; then
        tmp="$cache_root/.tmp-$pin-$$"
        rm -rf "$tmp"
        mkdir -p "$tmp"
        cp -a "$source_path"/. "$tmp"/
        chmod -R u+w "$tmp"
        printf '%s\n' "$pin" > "$tmp/.quasar-pin"
        rm -rf "$workdir"
        mv "$tmp" "$workdir"
      fi

      cd "$workdir"

      export QUASAR_DEV_NIX_READY=1
      export QUASAR_PRODUCTION_NIX_READY=1
      export QUASAR_PRODUCTION_SMOKE_NIX_READY=1
      export QUASAR_TEK9_PATH="''${QUASAR_TEK9_PATH:-$HOME/starintel/tek9}"
      export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath quasarRuntimeLibs}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
      export TMPDIR="/tmp"
      export TMP="/tmp"
      export TEMP="/tmp"
      export XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"
      export XDG_CACHE_HOME="''${XDG_CACHE_HOME:-$HOME/.cache}"
      export CL_SOURCE_REGISTRY="(:source-registry (:tree \"$QUASAR_TEK9_PATH/\") (:tree \"$HOME/quicklisp/local-projects/\") (:tree \"$HOME/quicklisp/dists/quicklisp/software/\") (:tree \"$PWD/systems/\") :ignore-inherited-configuration)"

      mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME"
      bash scripts/bootstrap-lisp-deps

      if [ ! -d node_modules ]; then
        npm ci
      fi

      exec npm run dev
    '';
  };
in
{
  home.packages = with pkgs; [
    nyxt
    quasarStart
    # Development
    gitRepo
    nodejs_24
    sqlitebrowser
    vim
    direnv
    sbcl
    pre-commit
    swi-prolog
    gnuplot
    ansible
    graphviz
    # Multimedia
    obs-studio
    # BUG kdePackages.kdenlive
    qbittorrent
    picard
    # System Tools
    gparted
    filezilla
    terminator
    remmina
    freerdp
    virt-manager
    virtiofsd

    # Productivity
    recoll
    w3m
    cht-sh
    kdePackages.kleopatra
    gimp
    feh
    activitywatch
    # FIXME nyxt
    remmina
    freerdp
    sqlitebrowser
    telegram-desktop
    virt-viewer
    kdePackages.kdeconnect-kde
    # Security
    keepassxc

    # Communication
    #vesktop
    discord
    element-desktop
    thunderbird
    # Misc
    # FIXME Broken package
    #monero-gui
    hugo
    #AI
    ollama
    # GUI Toolkit

    #emojione # wttr widget emojis
    #noto-fonts-emoji
    # TODO make a nixos module for qtile?
    # Can we do qtile without nixos module?
    conky
    j4-dmenu-desktop
    fetchmail
    variety
    file
    yt-dlp # For Emacs
    inputs.bixby-studio.packages.${pkgs.system}.default
  ];
}
