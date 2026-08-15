#!/usr/bin/env python
import os
import subprocess

import psutil
from libqtile import bar, hook, layout, widget
from libqtile.config import Drag, DropDown, Group, Key, KeyChord, Match, ScratchPad, Screen
from libqtile.lazy import lazy

mod = "mod4"
alt = "mod1"
home = os.path.expanduser("~")
wmname = "LG3D"


def read_ip(path=os.path.expanduser("~/.local/share/ip")):
    with open(path, encoding="utf-8") as stream:
        return stream.read().strip()


my_ip = read_ip()
colors = [
    ["#170c32"] * 2,
    ["#202146"] * 2,
    ["#92406e"] * 2,
    ["#fba922"] * 2,
    ["#2de2e6"] * 2,
    ["#f3f4f5"] * 2,
    ["#f6019d"] * 2,
    ["#62FF00"] * 2,
    ["#dd546e"] * 2,
    ["#9700cc"] * 2,
]


group_names = list("1234567890")
group_labels = ["", "", "", "", "", "", "", "", "", ""]
default_group_layouts = {
    name: "max" if name == "1" else "monadtall" for name in group_names
}
auto_group_routes = {
    "1": {
        "navigator",
        "firefox",
        "vivaldi-stable",
        "vivaldi-snapshot",
        "chromium",
        "google-chrome",
        "brave",
        "brave-browser",
    },
    "2": {"emacs", "codium"},
    "3": {"inkscape", "nomacs", "ristretto", "nitrogen", "feh", "gimp", "krita"},
    "4": {"virt-manager", "virtual machine manager"},
    "6": {"vlc", "mpv", "minecraft", "war thunder"},
    "8": {
        "thunar",
        "nemo",
        "caja",
        "nautilus",
        "org.gnome.nautilus",
        "pcmanfm",
        "pcmanfm-qt",
    },
    "9": {"evolution", "geary", "mail", "thunderbird"},
}
auto_group_mode = False
auto_group_buttons = []

groups = [
    Group(name=name, label=label, layout=default_group_layouts[name])
    for name, label in zip(group_names, group_labels)
]


def routed_group(window):
    try:
        wm_classes = window.window.get_wm_class() or ()
    except AttributeError:
        return None

    normalized = {value.casefold() for value in wm_classes if value}
    for group_name, classes in auto_group_routes.items():
        if normalized & classes:
            return group_name
    return None


def update_group_layout(group):
    if group.name not in default_group_layouts:
        return

    desired = "max" if len(group.windows) > 3 else default_group_layouts[group.name]
    if group.layout.name != desired:
        group.setlayout(desired)


def update_auto_layouts(qtile):
    for group_name in group_names:
        update_group_layout(qtile.groups_map[group_name])


def apply_auto_grouping(window):
    if not auto_group_mode or not getattr(window, "group", None):
        return
    if window.group.name not in group_names:
        return

    target = routed_group(window)
    if target and window.group.name != target:
        source = window.group.name
        from qtile_telemetry import telemetry_auto_route
        telemetry_auto_route(window, source, target)
        window.togroup(target, switch_group=False)

    update_auto_layouts(window.qtile)


def organize_existing_windows(qtile):
    for group_name in group_names:
        group = qtile.groups_map[group_name]
        for window in tuple(group.windows):
            target = routed_group(window)
            if target and target != group_name:
                from qtile_telemetry import telemetry_auto_route
                telemetry_auto_route(window, group_name, target)
                window.togroup(target, switch_group=False)
    update_auto_layouts(qtile)


def auto_group_button_colors():
    return "#000000", colors[7] if auto_group_mode else colors[8]


def update_auto_group_buttons():
    foreground, background = auto_group_button_colors()
    for button in auto_group_buttons:
        button.foreground = foreground
        button.background = background
        button.draw()


@lazy.function
def toggle_auto_group_mode(qtile):
    global auto_group_mode
    auto_group_mode = not auto_group_mode
    from qtile_telemetry import telemetry_event
    telemetry_event("auto_mode_changed", enabled=auto_group_mode)
    if auto_group_mode:
        organize_existing_windows(qtile)
    update_auto_group_buttons()


@hook.subscribe.startup_once
def startup_once():
    subprocess.call([home + "/.config/qtile/scripts/autostart.sh"])


@hook.subscribe.startup
def startup():
    subprocess.Popen(["xsetroot", "-cursor_name", "left_ptr"])


@hook.subscribe.client_managed
def auto_group_new_window(window):
    if auto_group_mode:
        window.qtile.call_soon(apply_auto_grouping, window)


@hook.subscribe.group_window_add
def enforce_auto_group(group, window):
    if auto_group_mode and group.name in group_names:
        group.qtile.call_soon(apply_auto_grouping, window)


@hook.subscribe.client_killed
def update_layout_after_close(window):
    if auto_group_mode:
        window.qtile.call_soon(update_auto_layouts, window.qtile)


@lazy.function
def kill_focused_window(qtile):
    if qtile.current_window:
        qtile.current_window.kill()


@lazy.function
def move_window_screen(qtile, step):
    if not qtile.current_window:
        return
    current = qtile.screens.index(qtile.current_screen)
    target = current + step
    if 0 <= target < len(qtile.screens):
        qtile.current_window.togroup(qtile.screens[target].group.name)
        qtile.focus_screen(target)


keys = [
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "q", kill_focused_window),
    Key([mod, "shift"], "q", kill_focused_window),
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "space", lazy.next_layout()),
    Key([mod, "shift"], "f", lazy.layout.flip()),
    Key([mod, "shift"], "space", lazy.window.toggle_floating()),
]
for key in ("Up", "Left", "k", "h"):
    keys.append(Key([mod], key, lazy.layout.previous()))
for key in ("Down", "Right", "j", "l"):
    keys.append(Key([mod], key, lazy.layout.next()))
for key, commands in {
    "l": (lazy.layout.grow_right(), lazy.layout.grow(), lazy.layout.increase_ratio(), lazy.layout.delete()),
    "Right": (lazy.layout.grow_right(), lazy.layout.grow(), lazy.layout.increase_ratio(), lazy.layout.delete()),
    "h": (lazy.layout.grow_left(), lazy.layout.shrink(), lazy.layout.decrease_ratio(), lazy.layout.add()),
    "Left": (lazy.layout.grow_left(), lazy.layout.shrink(), lazy.layout.decrease_ratio(), lazy.layout.add()),
    "k": (lazy.layout.grow_up(), lazy.layout.grow(), lazy.layout.decrease_nmaster()),
    "Up": (lazy.layout.grow_up(), lazy.layout.grow(), lazy.layout.decrease_nmaster()),
    "j": (lazy.layout.grow_down(), lazy.layout.shrink(), lazy.layout.increase_nmaster()),
    "Down": (lazy.layout.grow_down(), lazy.layout.shrink(), lazy.layout.increase_nmaster()),
}.items():
    keys.append(Key([mod, "control"], key, *commands))
for key, command in {
    "k": lazy.layout.flip_up(),
    "j": lazy.layout.flip_down(),
    "l": lazy.layout.flip_right(),
    "h": lazy.layout.flip_left(),
}.items():
    keys.append(Key([mod, alt], key, command))
for key, command in {
    "k": lazy.layout.shuffle_up(),
    "Up": lazy.layout.shuffle_up(),
    "j": lazy.layout.shuffle_down(),
    "Down": lazy.layout.shuffle_down(),
    "h": lazy.layout.shuffle_left(),
    "l": lazy.layout.shuffle_right(),
}.items():
    keys.append(Key([mod, "shift"], key, command))

keys.append(
    KeyChord(
        [mod],
        "e",
        [
            Key([], "e", lazy.spawn("emacsclient -c -a 'emacs'"), desc="Dashboard"),
            Key([], "a", lazy.spawn("emacsclient -c -a 'emacs' --eval '(emms)' --eval '(emms-play-directory-tree nsaspy/music-dir)'"), desc="EMMS"),
            Key([], "b", lazy.spawn("emacsclient -c -a 'emacs' --eval '(ibuffer)'"), desc="Ibuffer"),
            Key([], "d", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'"), desc="Dired"),
            Key([], "n", lazy.spawn("emacsclient -c -a 'emacs' --eval '(elfeed-update)' --eval '(elfeed)'"), desc="Elfeed"),
            Key([], "s", lazy.spawn("emacsclient -c -a 'emacs' --eval '(+eshell/here)'"), desc="Eshell"),
            Key([], "v", lazy.spawn("emacsclient -c -a 'emacs' --eval '(lish-vterm)'"), desc="Vterm"),
            Key([], "p", lazy.spawn("emacsclient -c -a 'emacs' --eval '(addmacs)'"), desc="Addmacs"),
            Key([], "y", lazy.spawn("emacsclient -c -a 'emacs' --eval '(+gptel/here)'"), desc="GPTel"),
        ],
        name="emacs",
    )
)
for group in groups:
    keys.extend(
        [
            Key([mod], group.name, lazy.group[group.name].toscreen()),
            Key([mod, "shift"], group.name, lazy.window.togroup(group.name), lazy.group[group.name].toscreen()),
        ]
    )
keys.extend(
    [
        Key([mod], "Tab", lazy.screen.next_group()),
        Key([mod, "shift"], "Tab", lazy.screen.prev_group()),
        Key([alt], "Tab", lazy.screen.next_group()),
        Key([alt, "shift"], "Tab", lazy.screen.prev_group()),
    ]
)


def dropdown(name, command, **kwargs):
    return DropDown(name, command, height=0.8, width=0.8, x=0.1, y=0.1, on_focus_lost_hide=False, **kwargs)


groups.extend(
    [
        ScratchPad("termpad", [DropDown("term", "terminator")]),
        ScratchPad("browserPad", [dropdown("browser", ["nyxt"])]),
        ScratchPad(
            "editorPad",
            [
                dropdown("emacs", home + "/.config/qtile/scripts/eclient.sh", match=Match(title="floating"), opacity=0.95),
                dropdown("org-capture", home + "/.config/qtile/scripts/org-capture.sh", match=Match(title="org-capture"), opacity=0.95),
            ],
        ),
        ScratchPad("passwords", [dropdown("keepassxc", "keepassxc", opacity=0.95)]),
        ScratchPad("media", [dropdown("feishin", "feishin", match=Match(wm_class="feishin"), opacity=0.95)]),
    ]
)
keys.extend(
    [
        Key([mod, "shift"], "F1", lazy.group["browserPad"].dropdown_toggle("browser")),
        Key([mod], "F12", lazy.group["termpad"].dropdown_toggle("term")),
        Key([mod, "shift"], "E", lazy.group["editorPad"].dropdown_toggle("emacs")),
        Key([mod], "F3", lazy.group["passwords"].dropdown_toggle("keepassxc")),
        Key([mod], "x", lazy.group["editorPad"].dropdown_toggle("org-capture")),
        Key([mod, "shift"], "M", lazy.group["media"].dropdown_toggle("feishin")),
        Key([alt], "Right", lazy.next_screen()),
        Key([alt], "Left", lazy.prev_screen()),
        Key([mod, "shift"], "Right", move_window_screen(1)),
        Key([mod, "shift"], "Left", move_window_screen(-1)),
    ]
)

theme = {"margin": 5, "border_width": 2, "border_focus": colors[2], "border_normal": colors[1]}
layouts = [layout.MonadTall(**theme), layout.MonadWide(**theme), layout.Matrix(**theme), layout.Bsp(**theme), layout.Floating(**theme), layout.RatioTile(**theme), layout.Max(**theme)]


auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = False
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False


@hook.subscribe.client_new
def swallow(window):
    pid = window.window.get_net_wm_pid()
    if pid is None:
        return
    try:
        parent_pid = psutil.Process(pid).ppid()
    except psutil.Error:
        return
    clients = {client.window.get_net_wm_pid(): wid for wid, client in window.qtile.windows_map.items()}
    for _ in range(5):
        if not parent_pid:
            return
        if parent_pid in clients:
            parent = window.qtile.windows_map.get(clients[parent_pid])
            if parent:
                parent.minimized = True
                window.parent = parent
            return
        try:
            parent_pid = psutil.Process(parent_pid).ppid()
        except psutil.Error:
            return


@hook.subscribe.client_killed
def unswallow(window):
    if getattr(window, "parent", None):
        window.parent.minimized = False


widget_defaults = {"font": "Hack Nerd Regular", "fontsize": 12, "padding": 2, "background": colors[1]}


def sep(padding=10):
    return widget.Sep(linewidth=1, padding=padding, foreground=colors[2], background=colors[1])


def auto_group_button():
    foreground, background = auto_group_button_colors()
    button = widget.TextBox(
        name=f"auto_group_mode_{len(auto_group_buttons)}",
        text="AUTO",
        font="Hack Nerd Regular",
        fontsize=12,
        padding=8,
        foreground=foreground,
        background=background,
        mouse_callbacks={"Button1": toggle_auto_group_mode},
    )
    auto_group_buttons.append(button)
    return button


def widgets():
    return [
        auto_group_button(),
        sep(5),
        widget.GroupBox(font="3270 Nerd Font", visible_groups=group_names, fontsize=18, margin_y=2, margin_x=2, padding_y=-6, padding_x=6, borderwidth=0, active=colors[9], inactive=colors[5], rounded=True, highlight_method="text", this_current_screen_border=colors[8], foreground=colors[2], background=colors[1]),
        sep(),
        widget.CurrentLayout(font="Hack Bold", foreground=colors[5], background=colors[1]),
        sep(),
        widget.WindowName(font="Hack", fontsize=12, foreground=colors[5], background=colors[1]),
        widget.Pomodoro(foreground=colors[2], background=colors[1]),
        sep(),
        widget.Mpris2(background=colors[1], foreground=colors[6], scroll_fixed_width=True, poll_interval=1, width=100, padding=10, size=60, linewidth=60, max_chars=60, markup=False),
        widget.GenPollCommand(cmd=["curl", "-s", "--max-time", "5", f"https://wttr.in/@{my_ip}?u&format=%f+%C"], parse=lambda output: output.strip() or "weather n/a", update_interval=300),
        sep(5),
        widget.ThermalSensor(foreground=colors[5], foreground_alert=colors[6], background=colors[1], metric=True, padding=3, threshold=70, tag_sensor="Tctl"),
        sep(),
        widget.Net(foreground=colors[6], background=colors[1]),
        widget.TextBox(font="FontAwesome", text="  ", foreground=colors[6], background=colors[1], padding=0, fontsize=16),
        widget.CPUGraph(border_color=colors[2], fill_color=colors[4], graph_color=colors[4], background=colors[1], border_width=1, line_width=1, core="all", type="box"),
        sep(),
        widget.Memory(font="Hack", format="Mem:{MemUsed: 0.2f}G", update_interval=1, fontsize=12, foreground=colors[6], background=colors[1], measure_mem="G"),
        sep(),
        widget.TextBox(font="FontAwesome", text="  ", foreground=colors[3], background=colors[1], padding=0, fontsize=16),
        widget.Clock(foreground=colors[5], background=colors[1], fontsize=12, format="%Y-%m-%d %H:%M", mouse_callbacks={"Button1": lambda: os.system('notify-send -a qtile "$(date "+%Y-%m-%d %H:%M")" "$(cal)"')}),
        sep(),
        widget.Volume(foreground=colors[2], background=colors[1], volume_up_command="amixer set Master 10%+", volume_down_command="amixer set Master 10%-"),
    ]


def screen_widgets(systray=False):
    items = widgets()
    if systray:
        items.append(widget.Systray(background=colors[1], icon_size=20, padding=4))
    return items


def generate_screens(output_info):
    tray_output = sorted(output_info, key=lambda output: output.rect.x)[
        len(output_info) // 2
    ]
    return [
        Screen(
            top=bar.Bar(
                screen_widgets(output is tray_output),
                26,
                opacity=0.8,
            )
        )
        for output in output_info
    ]


mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
]
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        *[Match(wm_class=name) for name in ("confirmreset", "makebranch", "maketag", "ssh-askpass", "Arcolinux-welcome-app.py", "Arcolinux-calamares-tool.py", "confirm", "dialog", "download", "error", "file_progress", "notification", "splash", "toolbar", "Arandr", "feh", "Galculator", "archlinux-logout", "xfce4-terminal")],
        *[Match(title=name) for name in ("branchdialog", "pinentry", "floating", "Minecraft", "Emacs Everywhere", "Atomic Chrome", "org-capture", "Feishin")],
    ],
    fullscreen_border_width=0,
    border_width=0,
)


from qtile_telemetry import install_telemetry
install_telemetry(globals())
