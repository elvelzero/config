# Imports
import os
import subprocess
from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from typing import List  # noqa: F401

# Autostart
@hook.subscribe.startup
def autostart():
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])

mod         = "mod4"
myTerminal  = "kitty"
myFile      = "spacefm"
aFile       = myTerminal + " -e ranger"
myBrowser   = "firefox"
aBrowser    = "firefox -private"
screenshot  = "scrot /home/sleepy/Pictures/Screenshots/scrot.png"
lock        = "./.config/i3lock-color/lock.sh"

keys = [
    # Qtile core
    Key([mod, "control"], "r",
        lazy.restart(), desc="Restart Qtile"
        ),
    Key([mod, "shift"], "q",
        lazy.shutdown(),
        desc="Shutdown Qtile"
        ),
    Key([mod], "r",
        # lazy.spawn("rofi -show drun -config ~/.config/rofi/themes/nord.rasi -display-drun \"Run: \" -drun-display-format \"{name}\" -show-icons"),
        lazy.spawn("rofi -show drun -config ~/.config/rofi/themes/solarized.rasi -display-drun \"Run: \" -drun-display-format \"{name}\" -show-icons"),
        desc="Spawn a command using a prompt widget"
        ),
    Key([mod, "shift"], "r",
        # lazy.spawn("rofi -show run -config ~/.config/rofi/themes/nord.rasi -display-run"),
        lazy.spawn("rofi -show run -display-run"),
        desc="Spawn a command using a prompt widget"
        ),
    Key([mod, "shift"], "c",
        lazy.window.kill(),
        desc="Kill focused window"
        ),
    Key([mod, "shift"], "s",
        lazy.spawn(lock),
        desc="Lock screen"
        ),
    Key([mod], "p",
        lazy.spawn(screenshot),
        desc="Screenshot"
        ),

    # Toggle window
    Key([mod], "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle fullscreen"
        ),
    Key([mod, "shift"], "f",
        lazy.window.toggle_floating(),
        desc="Toggle floating"
        ),

    # Toggle between different layouts
    # Key([mod], "space",
    Key([mod], "Tab",
        lazy.next_layout(),
        desc="Toggle between layouts"
        ),
    Key([mod, "shift"], "Tab",
        lazy.prev_layout(),
        desc="Toggle between layouts"
        ),

    # Switch between windows
    Key([mod], "h",
        lazy.layout.left(),
        desc="Move focus to left"
        ),
    Key([mod], "l",
        lazy.layout.right(),
        desc="Move focus to right"
        ),
    Key([mod], "j",
        lazy.layout.down(),
        desc="Move focus down"
        ),
    Key([mod], "k",
        lazy.layout.up(),
        desc="Move focus up"
        ),
    Key([mod], "space",
        lazy.layout.next(),
        desc="Move window focus to other window"
    ),

    # Move windows
    Key([mod, "shift"], "h",
        lazy.layout.shuffle_left(),
        desc="Move window to the left"
        ),
    Key([mod, "shift"], "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right"
        ),
    Key([mod, "shift"], "j",
        lazy.layout.shuffle_down(),
        desc="Move window down"
        ),
    Key([mod, "shift"], "k",
        lazy.layout.shuffle_up(),
        desc="Move window up"
        ),

    # Grow windows
    Key([mod, "control"], "h",
        lazy.layout.grow_left(),
        desc="Grow window to the left"
        ),
    Key([mod, "control"], "l",
        lazy.layout.grow_right(),
        desc="Grow window to the right"
        ),
    Key([mod, "control"], "j",
        lazy.layout.grow_down(),
        desc="Grow window down"
        ),
    Key([mod, "control"], "k",
        lazy.layout.grow_up(),
        desc="Grow window up"
        ),
    Key([mod], "n",
        lazy.layout.normalize(),
        desc="Reset all window sizes"
        ),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"
        ),

    # Launch Apps
    Key([mod], "Return",
        lazy.spawn(myTerminal),
        desc="Launch terminal"
        ),
    Key([mod], "e",
        lazy.spawn(myFile),
        desc="Launch file manager"
        ),
    Key([mod, "shift"], "e",
        lazy.spawn(aFile),
        desc="Launch file manager"
        ),    
    Key([mod], "b",
        lazy.spawn(myBrowser),
        desc="Launch browser"
        ),
    Key([mod, "shift"], "b",
        lazy.spawn(aBrowser),
        desc="Launch browser"
        ),
    Key([mod], "o",
        lazy.spawn("libreoffice"),
        desc="Launch document editor"
        ),
    Key([mod, "shift"], "o",
        lazy.spawn("zathura"),
        desc="Launch pdf viewer"
        ),

    Key([mod], "w",
        lazy.spawn("nitrogen"),
        desc="Launch nitrogen"
        ),
    Key([mod], "v",
        lazy.spawn("celluloid"),
        desc="Launch video player"
        ),
    Key([mod, "control"], "v",
        lazy.spawn("obs"),
        desc="Launch screen recorder"
        ),
    Key([mod, "shift"], "v",
        lazy.spawn("olive-editor"),
        desc="Launch video editor"
        ),
    Key([mod], "g",
        lazy.spawn("gimp"),
        desc="Launch raster image editor"
        ),
    Key([mod, "shift"], "g",
        lazy.spawn("inkscape"),
        desc="Launch vector image editor"
        ),


]

# workspace name
# Na = 나, Jeong = 정, Mo = 모, Sa = 사, Ji = 지, Mi = 미, Da = 다, Chae = 채, Tzu = 쯔

group_names = [("나", {'layout': 'columns'}),
               ("정", {'layout': 'columns'}),
               ("모", {'layout': 'columns'}),
               ("사", {'layout': 'columns'}),
               ("지", {'layout': 'columns'}),
               ("미", {'layout': 'columns'}),
               ("다", {'layout': 'columns'}),
               ("채", {'layout': 'columns'}),
               ("쯔", {'layout': 'floating'})]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group

layout_theme = {"border_width": 2,
                "margin": 7,
                # "border_focus": "#1496fa",
                "border_focus": "#63c5ea",
                "border_normal": "#3b4252"
                }

layouts = [
    # Try more layouts by unleashing below layouts.
    # layout.Stack(**layout_theme, num_stacks=2),
    # layout.Bsp(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.MonadTall(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Tile(**layout_theme),
    # layout.TreeTab(**layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Zoomy(**layout_theme),
    layout.Columns(**layout_theme),
    layout.Max(**layout_theme),
    layout.Floating(**layout_theme),
]

colors = [
            ["#00000000", "#00000000"],   # 0 transparent color
            # ["#282a36", "#282a36"], # 1 background color (dracula background)
            # ["#161821e6", "#161821e6"], # 1 backgroicebergor (iceberg background)
            # ["#161821", "#161821"], # 1 backgroicebergor (iceberg background)
            ["#2e3440e6", "#2e3440e6"], # 1 backgroicebergor (iceberg background)
            ["#eceff4", "#eceff4"], # 2 foreground color
            # ["#1496fa", "#1496fa"], # 3 Line highlight color
            ["#63c5ea", "#63c5ea"], # 3 Line highlight color
            ["#bf616a", "#bf616a"], # 4 Other line highlight color

         ] 

widget_defaults = dict(
    font='Fira Code',
    fontsize=12,
    padding=3,
)

extension_defaults = widget_defaults.copy()

def init_widgets_list():
    widgets_list = [
             widget.Sep(
                    linewidth   = 0,
                    padding     = 5,
                    background  = colors[1]
                ),
                widget.Image(
                    filename        = "~/.config/qtile/icons/blackarchlogo.png",
                    mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("rofi -show drun -config ~/.config/rofi/themes/launchpad.rasi -display-drun \"Run: \" -drun-display-format \"{name}\" -show-icons")},
                    background      = colors[1]
                ),
                widget.Sep(
                    linewidth   = 0,
                    padding     = 5,
                    background  = colors[1]
                ),
                widget.GroupBox(
                    font = "Baekmuk Batang Bold Italic",
                    fontsize = 11,
                    margin_y = 5,
                    margin_x = 0,
                    padding_y = 5,
                    padding_x = 3,
                    borderwidth = 2,
                    active = colors[2],
                    inactive = colors[3],
                    rounded = False,
                    highlight_color = colors[1],
                    highlight_method = "line",
                    this_current_screen_border = colors[3],
                    this_screen_border = colors [4],
                    background = colors[1]
                ),
                widget.Spacer(
                background  = colors[1]
                ),
                widget.Clock(
                    background  = colors[1],
                    font        = 'Cascadiacode Semibold Italic',
                    format      = "%A, %B %d - %H:%M"
                ),
                widget.Spacer(
                    background  = colors[1]
                ),
		        # CPU & Memory in widget box
                widget.WidgetBox(
                    foreground      = colors[3],
                    background      = colors[1],
                    font            = 'mononoki Nerd Font',
                    fontsize        = 16, 
                    text_closed     = '',
                    text_open       = '',
                    widgets=[
                        widget.CPU(
                           format      = '{load_percent}%',
                           mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerminal + " -e htop")},
                           foreground  = colors[2],
                           background  = colors[1]
                        ),
                        widget.TextBox(
                           font        = 'mononoki Nerd Font',
                           fontsize    = 12,   
                           text        = ',',
                           mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerminal + " -e htop")},
                           foreground  = colors[3],
                           background  = colors[1]
                        ),
                        widget.Memory(
                           format      = '{MemUsed:.0f}{mm}',
                           mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerminal + " -e htop")},
                           foreground  = colors[2],
                           background  = colors[1]
                        ),
                    ]
                ),
                # Battery widget
                widget.TextBox(
                   font        = 'mononoki Nerd Font',
                   fontsize    = 12, 
                   text        = ' ',
                   foreground  = colors[3],            # battery logo
                   background  = colors[1]
                ),
                widget.Battery(
                   charge_char     = '',
                   discharge_char  = '~',
                   empty_char      = '!',
                   full_char       = '',
                   update_interval = 1,
                   font            = 'mononoki Nerd Font',
                   fontsize        = 12,
                   format          = '{percent:2.0%} {char}',
                   foreground      = colors[2],
                   background      = colors[1]
                ),
                # Backlight Widget
                widget.Sep(
                    linewidth   = 0,
                    padding     = 3,
                    background  = colors[1]
                ),
                widget.TextBox(
                   font        = 'mononoki Nerd Font',
                   fontsize    = 12, 
                   text        = ' ',
                   foreground  = colors[3],            # backlight logo
                   background  = colors[1]
                ),
                widget.Backlight(
                    backlight_name  = "intel_backlight",
                    font            = 'mononoki Nerd Font',
                    fontsize        = 12,
                    # fmt             = ' {}',
                    foreground      = colors[2],
                    background      = colors[1]
                ),
                # Volume widget
                widget.Sep(
                    linewidth   = 0,
                    padding     = 3,
                    background  = colors[1]
                ),
                widget.TextBox(
                    font            = 'mononoki Nerd Font',
                    fontsize        = 12, 
                    text            = '',
                    mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerminal + " -e alsamixer")},
                    foreground      = colors[3],            # volume logo
                    background      = colors[1]
                ),
                widget.Volume(
                    font        = 'mononoki Nerd Font',
                    fontsize    = 12,
                    # fmt         = ' {}',
                    # mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerminal + " -e alsamixer")},
                    foreground  = colors[2],
                    background  = colors[1]
                ),
                # Systray in widget box
                widget.WidgetBox(
                    foreground      = colors[3],
                    background      = colors[1],
                    font            = 'mononoki Nerd Font',
                    fontsize        = 16, 
                    text_closed     = '',
                    text_open       = '',
                    close_button_location   = 'right',
                    widgets=[
                        widget.Systray(
                            foreground  = colors[2],
                            background  = colors[1]
                        )
                    ]
                ),
                # Screenshot, Current layout icon widget, and Lock
                widget.Sep(
                    linewidth   = 0,
                    padding     = 3,
                    background  = colors[1]
                ),
                widget.TextBox(
                   font        = 'mononoki Nerd Font',
                   fontsize    = 16,   
                   text        = '',
                   foreground  = colors[3],
                   background  = colors[1]
                ),
                widget.TextBox(                         # screenshot logo
                   font        = 'mononoki Nerd Font',
                   fontsize    = 14,
                   text        = ' ',
                   mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("scrot /home/sleepy/Pictures/Screenshots/%Y-%m-%d-%T-scrot.png")},
                   foreground  = colors[3],
                   background  = colors[1]
                ),
                widget.TextBox(                         # lock logo
                   font        = 'mononoki Nerd Font',
                   fontsize    = 14,
                   text        = '',
                   mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn("./.config/i3lock-color/lock.sh")},
                   foreground  = colors[3],
                   background  = colors[1]
                ),
                widget.TextBox(
                   font        = 'mononoki Nerd Font',
                   fontsize    = 16,   
                   text        = '',
                   foreground  = colors[3],
                   background  = colors[1]
                ),
                widget.Sep(
                    linewidth   = 0,
                    padding     = 2,
                    background  = colors[1]
                ),
                widget.CurrentLayoutIcon(
                    background = colors[1],
                    padding    = 3,
                    scale      = 0.7
                ),
                widget.Sep(
                    linewidth   = 0,
                    padding     = 3,
                    background  = colors[1]
                ),
    ]
    return widgets_list

def init_widgets_screen():
    widgets_screen = init_widgets_list()
    return widgets_screen                 # Monitor will display all widgets in widgets_list

def init_screens():
    return [Screen(
        top=bar.Bar(
            widgets         =init_widgets_screen(), 
            # opacity         =0.90, 
            opacity         =1.00, 
            size            =20,
            background      =colors[0],
            wallpaper_mode  = "fill",
            margin=[5, 5, 5, 5], # [top, right, bottom, left]
            ),
        bottom=bar.Gap(5),
        left=bar.Gap(5),
        right=bar.Gap(5),
        )
     ]

if __name__ in ["config", "__main__"]:
    screens = init_screens()
    widgets_list = init_widgets_list()
    widgets_screen = init_widgets_screen()

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(**layout_theme, float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    # Match(wm_class='code'),  # VS Code
    Match(wm_class='Places'),  # firefox download window
    Match(wm_class='gimp'),  # Gimp
    Match(wm_class='jetbrains-idea-ce'),  # intellij IDEA
    Match(wm_class='libreoffice'),  # LibreOffice
    Match(wm_class='PacketTracer'),  # CISCO Packet Tracer
    Match(wm_class='timeshift-gtk'),  # TimeShift
    # Match(wm_class='VirtualBox Manager'),  # VirtualBox
    # Match(wm_class='VirtualBox Machine'),  # VirtualMachine
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
