#!/usr/bin/env python
import os
import re
import socket
import subprocess
import psutil # For window swallow
from typing import List  # noqa: F401
from libqtile import layout, bar, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, Rule, KeyChord
from libqtile.config import ScratchPad, DropDown
from libqtile.lazy import lazy
from libqtile.widget import Spacer
from libqtile.log_utils import logger
from layouts.addmacs import AddmacsLayout
#import ip
def readIpFile(file_path= os.path.expanduser("~/.local/share/ip")):
    with open(file_path, "r") as file:
        return file.read().strip()

thermalTag="Tctl"

mod = "mod4"
mod1 = "mod1"
mod2 = "control"
home = os.path.expanduser('~')
myIp = readIpFile()
myTerm = "terminator"

#https://archive.is/DOJvD
wmname = "LG3D"
groups = []

def init_colors():
    return [["#170c32", "#170c32"], # color 0
            ["#202146", "#202146"], # color 1
            ["#92406e", "#92406e"], # color 2
            ["#fba922", "#fba922"], # color 3
            ["#2de2e6", "#2de2e6"], # color 4
            ["#f3f4f5", "#f3f4f5"], # color 5
            ["#f6019d", "#f6019d"], # color 6
            ["#62FF00", "#62FF00"], # color 7
            ["#dd546e", "#dd546e"], # color 8
            ["#9700cc", "#9700cc"]] # color 9


colors = init_colors()

@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/scripts/autostart.sh'])

@hook.subscribe.startup
def start_always():
    # Set the cursor to something sane in X
    subprocess.Popen(['xsetroot', '-cursor_name', 'left_ptr'])

group_names = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0",]
#group_labels = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0",]
group_labels = ["", "", "", "", "", "", "", "", "", "",]
group_layouts = ["max", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall", "monadtall",]

for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            layout=group_layouts[i].lower(),
            label=group_labels[i],
        ))

keys = [
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "q", lazy.window.kill()),
    Key([mod, "shift"], "q", lazy.window.kill()),
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "Up", lazy.layout.up()),
    Key([mod], "Down", lazy.layout.down()),
    Key([mod], "Left", lazy.layout.left()),
    Key([mod], "Right", lazy.layout.right()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        ),
    Key([mod, "control"], "Right",
        lazy.layout.grow_right(),
        lazy.layout.grow(),
        lazy.layout.increase_ratio(),
        lazy.layout.delete(),
        ),
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        ),
    Key([mod, "control"], "Left",
        lazy.layout.grow_left(),
        lazy.layout.shrink(),
        lazy.layout.decrease_ratio(),
        lazy.layout.add(),
        ),
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        ),
    Key([mod, "control"], "Up",
        lazy.layout.grow_up(),
        lazy.layout.grow(),
        lazy.layout.decrease_nmaster(),
        ),
    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        ),
    Key([mod, "control"], "Down",
        lazy.layout.grow_down(),
        lazy.layout.shrink(),
        lazy.layout.increase_nmaster(),
        ),
    Key([mod, "shift"], "f", lazy.layout.flip()),

    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "Left", lazy.layout.swap_left()),
    Key([mod, "shift"], "Right", lazy.layout.swap_right()),
    Key([mod, "shift"], "space", lazy.window.toggle_floating()),
    KeyChord([mod],"e", [
             Key([], "e",
                 lazy.spawn("emacsclient -c -a 'emacs'"),
                 desc='Emacsclient Dashboard'
                 ),
             Key([], "a",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(emms)' --eval '(emms-play-directory-tree nsaspy/music-dir)'"),
                 desc='Emacsclient EMMS (music)'
                 ),
             Key([], "b",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(ibuffer)'"),
                 desc='Emacsclient Ibuffer'
                 ),
             Key([], "d",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'"),
                 desc='Emacsclient Dired'
                 ),

             Key([], "n",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(elfeed-update)'  --eval '(elfeed)'"),
                 desc='Emacsclient Elfeed (RSS)'
                 ),
             Key([], "s",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(+eshell/here)'"),
                 desc='Emacsclient Eshell'
                 ),
             Key([], "v",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(lish-vterm)'"),
                 desc='Emacsclient Vterm'
                 ),
             Key([], "p",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(addmacs)'"),
                 desc='Emacsclient Vterm'
                 ),
             Key([], "y",
                 lazy.spawn("emacsclient -c -a 'emacs' --eval '(+gptel/here)'"),
                 desc='Emacsclient Vterm'
                 ),
         ])
 ]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen()),
        Key([mod], "Tab", lazy.screen.next_group()),
        Key([mod, "shift" ], "Tab", lazy.screen.prev_group()),
        Key(["mod1"], "Tab", lazy.screen.next_group()),
        Key(["mod1", "shift"], "Tab", lazy.screen.prev_group()),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name) , lazy.group[i.name].toscreen()),
    ])

groups.extend([ScratchPad("termpad", [
    DropDown("term",
             myTerm)]),
    ScratchPad("browserPad", [
    DropDown("browser",
             ["nyxt"],
             height=0.8,
             width = 0.8,
             x = 0.1,
             y = 0.1,
             on_focus_lost_hide=False)]),

    ScratchPad("editorPad", [
    DropDown("emacs",
             home + '/.config/qtile/scripts/eclient.sh',
             match = Match(title='floating'),
             height=0.8,
             width = 0.8,
             x = 0.1,
             y = 0.1,
             opacity = 0.95,
             on_focus_lost_hide=False),
    DropDown("org-capture",
             home + '/.config/qtile/scripts/org-capture.sh',
             match = Match(title='org-capture'),
             height=0.8,
             width = 0.8,
             x = 0.1,
             y = 0.1,
             opacity = 0.95,
             on_focus_lost_hide=False)

    ]),
    ScratchPad("passwords", [
    DropDown("keepassxc",
             "keepassxc",

             height=0.8,
             width = 0.8,
             x = 0.1,
             y = 0.1,
             opacity = 0.95,
             on_focus_lost_hide=False),
    ]),
    ScratchPad("media", [
    DropDown("sonixd",
             "sonixd",
             match = Match(wm_class='sonixd'),
             height=0.8,
             width = 0.8,
             x = 0.1,
             y = 0.1,
             opacity = 0.95,
             on_focus_lost_hide=False),
    ]),


               ])

keys.extend([Key([mod, "shift"], 'F1', lazy.group['browserPad'].dropdown_toggle('browser'))])
keys.extend([Key([mod], 'F12', lazy.group['termpad'].dropdown_toggle('term'))])
keys.extend([Key([mod, "shift"], "E", lazy.group['editorPad'].dropdown_toggle('emacs'))])
keys.extend([Key([mod], "F3", lazy.group['passwords'].dropdown_toggle('keepassxc'))])
keys.extend([Key([mod], "x", lazy.group['editorPad'].dropdown_toggle('org-capture'))])
keys.extend([Key([mod, "shift"], "M", lazy.group['media'].dropdown_toggle('sonixd'))])

def init_layout_theme():
    return {"margin":5,
            "border_width":2,
            "border_focus": colors[2],
            "border_normal": colors[1]
            }
layout_theme = init_layout_theme()
layouts = [
    #layout.MonadTall(margin=8, border_width=2, border_focus="#5e81ac", border_normal="#4c566a"),
    layout.MonadTall(**layout_theme),
    #layout.MonadWide(margin=8, border_width=2, border_focus="#5e81ac", border_normal="#4c566a"),
    layout.MonadWide(**layout_theme),
    layout.Matrix(**layout_theme),
    layout.Bsp(**layout_theme),
    layout.Floating(**layout_theme),
    layout.RatioTile(**layout_theme),
    layout.Max(**layout_theme)
]

@hook.subscribe.client_new
def assign_app_group(client):
     d = {}
     #####################################################################################
     ### Use xprop fo find  the value of WM_CLASS(STRING) -> First field is sufficient ###
     #####################################################################################
     d[group_names[0]] = ["Navigator", "Firefox", "Vivaldi-stable", "Vivaldi-snapshot", "Chromium", "Google-chrome", "Brave", "Brave-browser",
               "navigator", "firefox", "vivaldi-stable", "vivaldi-snapshot", "chromium", "google-chrome", "brave", "brave-browser", ]
     d[group_names[1]] = [ "emacs", "codium" ]
     d[group_names[2]] = ["Inkscape", "Nomacs", "Ristretto", "Nitrogen", "Feh",
                          "inkscape", "nomacs", "ristretto", "nitrogen", "feh", "gimp", "krita" ]
     d[group_names[3]] = ["virt-manager", "Virtual Machine Manager" ]
     #d[group_names[4]] = ["Meld", "meld", "org.gnome.meld" "org.gnome.Meld" ]
     d[group_names[5]] = ["Vlc","vlc", "Mpv", "mpv", "Minecraft", "War Thuder" ]
     #d[group_names[6]] = ["VirtualBox Manager", "VirtualBox Machine", "Vmplayer",
     #          "virtualbox manager", "virtualbox machine", "vmplayer", ]
     d[group_names[7]] = ["Thunar", "Nemo", "Caja", "Nautilus", "org.gnome.Nautilus", "Pcmanfm", "Pcmanfm-qt",
               "thunar", "nemo", "caja", "nautilus", "org.gnome.nautilus", "pcmanfm", "pcmanfm-qt", ]
     d[group_names[8]] = ["Evolution", "Geary", "Mail", "Thunderbird",
               "evolution", "geary", "mail", "thunderbird" ]
     #d[group_names[9]] = ["Spotify", "Pragha", "Clementine", "Deadbeef", "Audacious",
     #          "spotify", "pragha", "clementine", "deadbeef", "audacious" ]
     #     ######################################################################################
     m_class = client.window.get_wm_class()[0]

     for i in range(len(d)):
         if wm_class in list(d.values())[i]:
             group = list(d.keys())[i]
             client.togroup(group)
             client.group.cmd_toscreen(toggle=True)


main = None

@lazy.function
def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

@lazy.function
def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

def window_to_previous_screen(qtile, switch_group=False, switch_screen=False):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group, switch_group=switch_group)
        if switch_screen == True:
            qtile.cmd_to_screen(i - 1)

def window_to_next_screen(qtile, switch_group=False, switch_screen=False):
    i = qtile.screens.index(qtile.current_screen)
    if i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group, switch_group=switch_group)
        if switch_screen == True:
            qtile.cmd_to_screen(i + 1)

keys.extend([
    Key([mod1], "Right", lazy.next_screen(), desc="Next Monitor"),
    Key([mod1], "Left", lazy.prev_screen(), desc="Prev Monitor")
])

keys.extend([
    # MOVE WINDOW TO NEXT SCREEN
    Key([mod,"shift"], "Right", lazy.function(window_to_next_screen, switch_screen=True)),
    Key([mod,"shift"], "Left", lazy.function(window_to_previous_screen, switch_screen=True)),
])

auto_fullscreen = False # I dont want it "full screen"
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = False

follow_mouse_focus = True
bring_front_click = False
cursor_warp = True # Keep mouse inside game window!

@hook.subscribe.client_new
def _swallow(window):
    pid = window.window.get_net_wm_pid()
    ppid = psutil.Process(pid).ppid()
    cpids = {c.window.get_net_wm_pid(): wid for wid, c in window.qtile.windows_map.items()}
    for i in range(5):
        if not ppid:
            return
        if ppid in cpids:
            parent = window.qtile.windows_map.get(cpids[ppid])
            parent.minimized = True
            window.parent = parent
            return
        ppid = psutil.Process(ppid).ppid()

@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, 'parent'):
        window.parent.minimized = False

def init_widgets_defaults():
    return dict(font="Hack Nerd Regular",
                fontsize = 12,
                padding = 2,
                background=colors[1])

widget_defaults = init_widgets_defaults()

def init_widgets_list():
    prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
    widgets_list = [
        widget.GroupBox(font="3270 Nerd Font",
                        visible_groups=["1","2","3","4","5","6","7","8", "9", "0"],
                        fontsize = 18,
                        margin_y = 2,
                        margin_x = 2,
                        padding_y = -6,
                        padding_x = 6,
                        borderwidth = 0,
                        disable_drag = False,
                        active = colors[9],
                        inactive = colors[5],
                        rounded = True,
                        highlight_method = "text",
                        this_current_screen_border = colors[8],
                        foreground = colors[2],
                        background = colors[1]
                        ),
        widget.Sep(
            margin_x = 5,
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.CurrentLayout(
            font = "Hack  Bold",
            foreground = colors[5],
            background = colors[1]
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        widget.WindowName(font="Hack ",
                          fontsize = 12,
                          foreground = colors[5],
                          background = colors[1],
                          ),
        # widget.Net(
        #          font="Hack ",
        #          fontsize=12,
        #          interface="enp0s31f6",
        #          foreground=colors[2],
        #          background=colors[1],
        #          padding = 0,
        #          ),

        widget.Pomodoro(foreground = colors[2],
            background = colors[1],
            ),
        widget.Sep(
                  linewidth = 1,
                  padding = 10,
                  foreground = colors[2],
                  background = colors[1]
                  ),
        widget.Mpris2(background=colors[1],
                      foreground=colors[6],
                      scroll_fixed_width=True,
                      poll_interval=1,
                      width=100,
                      padding=10,
                      size=60,
                      linewidth = 60,
                      max_chars=60
                      ),
        widget.Wttr(
            format =  '%f %C',
            location={f'@{myIp}': 'Home'},
            units = "u",
            update_interval = 300,
        ),
        widget.Sep(
            linewidth = 1,
            padding = 5,
            foreground = colors[2],
            background = colors[1]
        ),
        ## do not activate in Virtualbox - will break qtile
        ## NOTE is this really the case for libvirt? lol
        widget.ThermalSensor(
            foreground = colors[5],
            foreground_alert = colors[6],
            background = colors[1],
            metric = True,
            padding = 3,
            threshold = 70,
            tag_sensor=thermalTag
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        # # battery option 1  ArcoLinux Horizontal icons do not forget to import arcobattery at the top
        # widget.Sep(
        #          linewidth = 1,
        #          padding = 10,
        #          foreground = colors[2],
        #          background = colors[1]
        #          ),
        # arcobattery.BatteryIcon(
        #          padding=0,
        #          scale=0.7,
        #          y_poss=2,
        #          theme_path=home + "/.config/qtile/icons/battery_icons_horiz",
        #          update_interval = 5,
        #          background = colors[1]
        #          ),
        # # battery option 2  from Qtile
        # widget.Sep(
        #          linewidth = 1,
        #          padding = 10,
        #          foreground = colors[2],
        #          background = colors[1]
        #          ),
        # widget.Battery(
        #          font="Hack ",
        #          update_interval = 10,
        #          fontsize = 12,
        #          foreground = colors[5],
        #          background = colors[1],
	    #          ),


        widget.Net(
            forground = colors[6],
            background=colors[1]
        ),

        widget.TextBox(
            font="FontAwesome",
            text="  ",
            foreground=colors[6],
            background=colors[1],
            padding = 0,
            fontsize=16
        ),
        widget.CPUGraph(
            border_color = colors[2],
            fill_color = colors[4],
            graph_color = colors[4],
            background=colors[1],
            border_width = 1,
            line_width = 1,
            core = "all",
            type = "box"
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        # widget.TextBox(
        #          font="FontAwesome",
        #          text="  ",
        #          foreground=colors[4],
        #          background=colors[1],
        #          padding = 0,
        #          fontsize=16
        #          ),
        widget.Memory(
            font="Hack ",
            format = 'Mem:{MemUsed: 0.2f}G',
            update_interval = 1,
            fontsize = 12,
            foreground = colors[6],
            background = colors[1],
            measure_mem = "G",
        ),
        widget.Sep(
            linewidth = 1,
            padding = 10,
            foreground = colors[2],
            background = colors[1]
        ),
        #widget.CheckUpdates(
        #         distro='Arch_yay',
        #         display_format=' {updates}',
        #         colour_no_update="#39ff14",
        #         colour_have_updates="#ff0000",
        #         update_interval=30,
        #         ),

        widget.TextBox(
            font="FontAwesome",
            text="  ",
            foreground=colors[3],
            background=colors[1],
            padding = 0,
            fontsize=16
        ),
        widget.Clock(
            foreground = colors[5],
            background = colors[1],
            fontsize = 12,
            format="%Y-%m-%d %H:%M"
        ),
        widget.Sep(
                  linewidth = 1,
                  padding = 10,
                  foreground = colors[2],
                  background = colors[1]
                  ),
        widget.Volume(
            #emoji="󱄠",
            foreground = colors[2],
            background = colors[1],
            volume_up_command = "amixer set Master 10%+",
            volume_down_command = "amixer set Master 10%-",
        )
    ]
    return widgets_list

widgets_list = init_widgets_list()

widgets_list = init_widgets_list()


def init_widgets_screen1():
    widgets_screen1 = init_widgets_list()

    widgets_screen1.append(widget.Systray(
                        background=colors[1],
                        icon_size=20,
                        padding = 4))
    return widgets_screen1

def init_widgets_screen2():
    widgets_screen2 = init_widgets_list()
    widget_len = len(widgets_screen2) - 10

    return widgets_screen2

widgets_screen1 = init_widgets_screen1()
widgets_screen2 = init_widgets_screen2()


def init_screens():
    return [Screen(top=bar.Bar(widgets=init_widgets_screen1(), size=26, opacity=0.8)),
            Screen(top=bar.Bar(widgets=init_widgets_screen2(), size=26, opacity=0.8))]
screens = init_screens()

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size())
]

@hook.subscribe.client_new
def set_floating(window):
    if (window.window.get_wm_transient_for()
            or window.window.get_wm_type() in floating_types):
        window.floating = True

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
    Match(wm_class='Arcolinux-welcome-app.py'),
    Match(wm_class='Arcolinux-calamares-tool.py'),
    Match(wm_class='confirm'),
    Match(wm_class='dialog'),
    Match(wm_class='download'),
    Match(wm_class='error'),
    Match(wm_class='file_progress'),
    Match(wm_class='notification'),
    Match(wm_class='splash'),
    Match(wm_class='toolbar'),
    Match(wm_class='Arandr'),
    Match(wm_class='feh'),
    Match(wm_class='Galculator'),
    Match(wm_class='archlinux-logout'),
    Match(wm_class='xfce4-terminal'),
    Match(title='floating'),
    Match(title='Minecraft'),
    Match(title='Emacs Everywhere'),
    Match(title='Atomic Chrome'),
    Match(title="org-capture"),
    Match(title="Sonixd")

],  fullscreen_border_width = 0, border_width = 0)



#groups.append(Group("project", layouts=[AddmacsLayout(margin=5)], matches=[Match(wm_class=re.compile(r"^(Emacs)$"))]))
#@hook.subscribe.client_new
#def assign_app_group(client):
#    if isinstance(client.window.get_wm_class(), tuple):
#        if any(x in client.name for x in ["Right Now", "In Progress", "Task Pile"]):
#            client.togroup("project")
