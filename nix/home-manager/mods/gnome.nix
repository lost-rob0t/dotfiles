{
  config,
  lib,
  ...
}:
let
  cfg = config.gnome;
  outrunGtkCss = ''
    @define-color outrun_bg #170c32;
    @define-color outrun_surface #202146;
    @define-color outrun_surface_hover #2a2056;
    @define-color outrun_fg #f3f4f5;
    @define-color outrun_magenta #ff00a8;
    @define-color outrun_cyan #00f3ff;
    @define-color outrun_purple #7c3aed;
    @define-color outrun_border #92406e;

    window,
    .background,
    preferencespage,
    preferencesgroup,
    clamp,
    scrolledwindow,
    viewport {
      background-color: @outrun_bg;
      color: @outrun_fg;
    }

    list,
    .boxed-list,
    .card,
    .content {
      background-color: @outrun_surface;
      color: @outrun_fg;
    }

    row,
    list row,
    .boxed-list row,
    preferencesgroup row {
      background-color: @outrun_surface;
      color: @outrun_fg;
    }

    row:hover,
    list row:hover,
    .boxed-list row:hover {
      background-color: @outrun_surface_hover;
    }

    row:selected,
    list row:selected,
    .boxed-list row:selected {
      background-color: @outrun_purple;
      color: @outrun_fg;
    }

    headerbar,
    .titlebar {
      background-color: @outrun_bg;
      color: @outrun_fg;
      border-color: @outrun_border;
    }

    button,
    entry,
    spinbutton,
    dropdown {
      background-color: @outrun_surface;
      color: @outrun_fg;
      border-color: @outrun_border;
    }

    button.suggested-action {
      background-color: @outrun_magenta;
      color: @outrun_bg;
    }

    button:focus,
    entry:focus,
    row:focus {
      outline-color: @outrun_cyan;
    }
  '';
in
{
  options.gnome.enable = lib.mkEnableOption "GNOME Outrun theming";

  config = lib.mkIf cfg.enable {
    dconf = {
      enable = true;
      settings."org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        gtk-theme = "Adwaita-dark";
      };
    };

    xdg.configFile."gtk-3.0/gtk.css".text = outrunGtkCss;
    xdg.configFile."gtk-4.0/gtk.css".text = outrunGtkCss;
  };
}
