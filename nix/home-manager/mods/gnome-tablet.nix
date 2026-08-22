{ config, lib, pkgs, ... }:

let
  cfg = config.desktop.gnomeTablet;
  uuid = "osk-terminal-avoid@lost-rob0t";
  extensionDir = ".local/share/gnome-shell/extensions/${uuid}";
in
{
  options.desktop.gnomeTablet.enable = lib.mkEnableOption
    "GNOME tablet-mode helpers";

  config = lib.mkIf cfg.enable {
    home.file."${extensionDir}/extension.js".source = ./gnome-tablet/extension.js;
    home.file."${extensionDir}/metadata.json".source = ./gnome-tablet/metadata.json;

    home.activation.enableGnomeTabletExtension =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        gsettings_bin=${pkgs.glib}/bin/gsettings
        uuid='${uuid}'

        current="$($gsettings_bin get org.gnome.shell enabled-extensions 2>/dev/null || true)"
        case "$current" in
          *"$uuid"*)
            ;;
          ""|"[]"|"@as []")
            $gsettings_bin set org.gnome.shell enabled-extensions "['$uuid']" || true
            ;;
          \[*\])
            next="''${current%]}"
            $gsettings_bin set org.gnome.shell enabled-extensions "$next, '$uuid']" || true
            ;;
        esac

        if command -v gnome-extensions >/dev/null 2>&1; then
          gnome-extensions enable "$uuid" >/dev/null 2>&1 || true
        fi
      '';
  };
}
