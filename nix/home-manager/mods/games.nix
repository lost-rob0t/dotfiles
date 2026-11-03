{
  lib,
  pkgs,
  inputs,
  config,
  ...
}:
let
  # Qtile IPC helpers for streaming.  The qtile client must match the
  # system qtile server, so /usr/bin/qtile is called by absolute path.
  # prep: ensure the game's group exists, focus the streamed screen,
  # switch it to the group, evict any other windows from it, and warp
  # the pointer onto the streamed screen — qtile maps new windows on
  # whichever screen owns the pointer, so without the warp the game
  # follows the user's mouse into their working group.  The original
  # pointer position is saved for the stream-end restore.
  qtileGamingPrep = pkgs.writeShellApplication {
    name = "qtile-gaming-prep";
    runtimeInputs = [ pkgs.jq pkgs.xdotool ];
    text = ''
      group="''${1:?usage: qtile-gaming-prep GROUP}"
      qt=/usr/bin/qtile

      "$qt" cmd-obj -o cmd -f addgroup -a "$group" >/dev/null 2>&1 || true
      "$qt" cmd-obj -o cmd -f to_screen -a 0 >/dev/null 2>&1 || true
      "$qt" cmd-obj -o cmd -f switchgroup -a "$group" >/dev/null 2>&1 || true

      "$qt" cmd-obj -o cmd -f windows \
        | jq -r --arg g "$group" '.[] | select(.group == $g) | .id' \
        | while read -r wid; do
            "$qt" cmd-obj -o window "$wid" -f kill >/dev/null 2>&1 || true
          done

      xdotool getmouselocation --shell \
        > "''${XDG_RUNTIME_DIR:-/tmp}/qtile-gaming-pointer-$group" 2>/dev/null || true
      xy=$("$qt" cmd-obj -o screen 0 -f info 2>/dev/null \
        | jq -r '.rect | "\(.x + .width / 2 | floor) \(.y + .height / 2 | floor)"')
      xdotool mousemove $xy
    '';
  };

  # Stream-end undo: put the pointer back where the user had it before
  # the prep warp so local work is not disturbed.
  qtileGamingPointerRestore = pkgs.writeShellApplication {
    name = "qtile-gaming-pointer-restore";
    runtimeInputs = [ pkgs.jq pkgs.xdotool ];
    text = ''
      group="''${1:?usage: qtile-gaming-pointer-restore GROUP}"
      state="''${XDG_RUNTIME_DIR:-/tmp}/qtile-gaming-pointer-$group"
      if [ -f "$state" ]; then
        x=$(grep '^X=' "$state" | cut -d= -f2)
        y=$(grep '^Y=' "$state" | cut -d= -f2)
        [ -n "$x" ] && [ -n "$y" ] && xdotool mousemove "$x" "$y"
        rm -f "$state"
      fi
      exit 0
    '';
  };

  # Runs detached next to the game and polls until its window maps on
  # the group, then true-fullscreens it (bar-free).  Never fatal: a game
  # that refuses fullscreen just stays windowed-maximized.
  qtileGamingFullscreen = pkgs.writeShellApplication {
    name = "qtile-gaming-fullscreen";
    runtimeInputs = [ pkgs.jq ];
    text = ''
      group="''${1:?usage: qtile-gaming-fullscreen GROUP}"
      qt=/usr/bin/qtile

      deadline=$((SECONDS + 90))
      while [ "$SECONDS" -lt "$deadline" ]; do
        wid=$("$qt" cmd-obj -o cmd -f windows 2>/dev/null \
          | jq -r --arg g "$group" '.[] | select(.group == $g) | .id' | head -1)
        if [ -n "$wid" ]; then
          sleep 2
          "$qt" cmd-obj -o window "$wid" -f enable_fullscreen >/dev/null 2>&1 && exit 0
        fi
        sleep 2
      done
      exit 0
    '';
  };

  # App launcher list shown in Moonlight.  Steam titles launch through
  # steam:// URLs so they share the running Steam client and its Proton
  # setup; Sunshine kills a stream's children on exit, so every game is
  # started detached and keeps running when the phone disconnects.
  # Each game gets its own qtile group: prep moves the streamed screen
  # to it and clears it, the watcher fullscreens the game once it maps.
  sunshineAppsJson = builtins.toJSON {
    env = {
      PATH = "$(PATH):$(HOME)/.local/bin:/usr/bin";
    };
    apps = [
      {
        name = "Desktop";
        image-path = "desktop.png";
      }
      {
        name = "Steam Big Picture";
        detached = [
          "setsid steam steam://open/bigpicture"
          "${qtileGamingFullscreen}/bin/qtile-gaming-fullscreen game-steam"
        ];
        prep-cmd = [
          {
            do = "${qtileGamingPrep}/bin/qtile-gaming-prep game-steam";
            undo = "setsid steam steam://close/bigpicture; ${qtileGamingPointerRestore}/bin/qtile-gaming-pointer-restore game-steam";
          }
        ];
        image-path = "steam.png";
      }
      {
        name = "War Thunder";
        detached = [
          "setsid steam steam://rungameid/236390"
          "${qtileGamingFullscreen}/bin/qtile-gaming-fullscreen game-warthunder"
        ];
        prep-cmd = [
          {
            do = "${qtileGamingPrep}/bin/qtile-gaming-prep game-warthunder";
            undo = "${qtileGamingPointerRestore}/bin/qtile-gaming-pointer-restore game-warthunder";
          }
        ];
      }
      {
        name = "Minecraft (Better MC BMC4)";
        detached = [
          "setsid prismlauncher --launch 'Better MC [FORGE] BMC4'"
          "${qtileGamingFullscreen}/bin/qtile-gaming-fullscreen game-bmc4"
        ];
        prep-cmd = [
          {
            do = "${qtileGamingPrep}/bin/qtile-gaming-prep game-bmc4";
            undo = "${qtileGamingPointerRestore}/bin/qtile-gaming-pointer-restore game-bmc4";
          }
        ];
      }
      {
        name = "Minecraft (Main)";
        detached = [
          "setsid prismlauncher --launch 'Main'"
          "${qtileGamingFullscreen}/bin/qtile-gaming-fullscreen game-main"
        ];
        prep-cmd = [
          {
            do = "${qtileGamingPrep}/bin/qtile-gaming-prep game-main";
            undo = "${qtileGamingPointerRestore}/bin/qtile-gaming-pointer-restore game-main";
          }
        ];
      }
      {
        name = "Minecraft (RLCraft)";
        detached = [
          "setsid prismlauncher --launch 'RLCraft'"
          "${qtileGamingFullscreen}/bin/qtile-gaming-fullscreen game-rlcraft"
        ];
        prep-cmd = [
          {
            do = "${qtileGamingPrep}/bin/qtile-gaming-prep game-rlcraft";
            undo = "${qtileGamingPointerRestore}/bin/qtile-gaming-pointer-restore game-rlcraft";
          }
        ];
      }
    ];
  };

  # Quality tuning for this host: qtile/X11, AMD RX 5500 (RDNA1), PipeWire.
  # The RDNA1 encode block does H.264 and HEVC (no AV1); HEVC delivers
  # visually lossless 1080p60 at the bitrates a phone can decode over
  # WiFi.  Actual bitrate/fps/codec are chosen by the Moonlight client.
  # Verified against sunshine 2026.914.233613 on this host:
  #   - output_name works for capture = x11 too and streams one display
  #     instead of the whole 5760x1080 virtual desktop;
  #   - capture = kms hard-exits without CAP_SYS_ADMIN, and a setcap
  #     capability does not survive nix store updates made by
  #     home-manager-updater, so x11 capture is the pinned default;
  #   - adapter and codec are unrecognized options in this version.
  sunshineConf = ''
    # Sunshine streaming host - managed by Home Manager
    # (nix/home-manager/mods/games.nix).  Edits made here or through the
    # web UI are overwritten at the next activation; change the module
    # source instead.

    # Primary monitor (xrandr DisplayPort-1).  Change output_name to
    # stream a different display.
    capture = x11
    output_name = DisplayPort-1

    # Encode on the AMD GPU rather than falling back to software.
    encoder = vaapi

    # CSRF guard (Sunshine 2026.516+): only localhost is allowed by
    # default, so opening the web UI via the LAN IP (even on this host)
    # blocks every PIN/save action.  Allowlist the LAN address.
    csrf_allowed_origins = https://10.50.50.28,https://10.50.50.28:47990

    # The host is a LAN service; no router hole punching.
    origin_web_ui_allowed = lan
    upnp = off
    min_log_level = 3
  '';
in
{
  options = with lib; {
    gaming = {
      enable = mkOption {
        type = types.bool;
        description = "Adds gaming packages with the Steam client and Moonlight/Sunshine game streaming.";
        default = false;
      };

      steam.enable = mkOption {
        type = types.bool;
        description = "Installs the Steam client (unfree). Home Manager master removed programs.steam, so this uses the nixpkgs FHS package. Terraria and War Thunder are Steam-distributed titles.";
        default = true;
      };

      moonlight.enable = mkOption {
        type = types.bool;
        description = "Installs the Moonlight Qt game streaming client.";
        default = true;
      };

      sunshine.enable = mkOption {
        type = types.bool;
        description = ''
          Manages the Sunshine game streaming host: a declarative
          ~/.config/sunshine/sunshine.conf tuned for this machine's AMD
          VAAPI encoder, a declarative apps.json (Desktop, Steam Big
          Picture, War Thunder, Prism Launcher Minecraft instances), and
          a graphical-session systemd user service.  Sunshine itself is
          installed whenever gaming.enable is set.
        '';
        default = true;
      };
    };
  };

  config =
    with lib;
    mkMerge [
      (mkIf config.gaming.enable {
        home.packages =
          with pkgs;
          [
            lutris
            steam-run
            wineWow64Packages.staging
            winetricks
            faudio
            qjoypad
            sunshine
          ]
          ++ [ inputs.mousetrap.packages.x86_64-linux.default ];
      })

      (mkIf (config.gaming.enable && config.gaming.moonlight.enable) {
        home.packages = with pkgs; [ moonlight-qt ];
      })

      (mkIf (config.gaming.enable && config.gaming.steam.enable) {
        home.packages = with pkgs; [
          steam
          protontricks
        ];
      })

      (mkIf (config.gaming.enable && config.gaming.sunshine.enable) {
        # Declarative Sunshine state.  These paths must not exist as
        # foreign regular files before the first activation (the sample
        # files Sunshine writes on first run are safely removable);
        # afterwards Home Manager owns them and web UI edits are reset
        # on every switch.
        xdg.configFile."sunshine/apps.json".text = sunshineAppsJson;

        xdg.configFile."sunshine/sunshine.conf".text = sunshineConf;

        systemd.user.services.sunshine = {
          Unit = {
            Description = "Sunshine game streaming host";
            After = [ "graphical-session.target" ];
            PartOf = [ "graphical-session.target" ];
          };
          Service = {
            ExecStart = "${lib.getExe pkgs.sunshine} %h/.config/sunshine/sunshine.conf";
            Restart = "on-failure";
            RestartSec = "5s";
          };
          Install = {
            WantedBy = [ "graphical-session.target" ];
          };
        };
      })
    ];
}
