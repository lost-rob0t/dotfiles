"""OpenRouter status widget integration and small Qtile runtime helpers."""

from libqtile import widget
from libqtile.config import Key
from libqtile.lazy import lazy

SYNC_AND_RELOAD_COMMAND = (
    'git-sync "$HOME/.dotfiles" && '
    "qtile cmd-obj -o root -f reload_config"
)


def _status_widget(home, colors):
    return widget.GenPollCommand(
        name="openrouter_status",
        cmd=["python3", home + "/.config/qtile/scripts/openrouter_status.py"],
        update_interval=15,
        markup=True,
        font="Hack Nerd Regular",
        fontsize=12,
        padding=4,
        foreground=colors[5],
        background=colors[1],
    )


def _install_sync_and_reload_key(config_globals):
    keys = config_globals.get("keys")
    mod = config_globals.get("mod", "mod4")
    if not isinstance(keys, list):
        return

    modifiers = {mod, "control", "shift"}
    if any(
        getattr(binding, "key", None) == "r"
        and set(getattr(binding, "modifiers", ())) == modifiers
        for binding in keys
    ):
        return

    keys.append(
        Key(
            [mod, "control", "shift"],
            "r",
            lazy.spawn(["bash", "-lc", SYNC_AND_RELOAD_COMMAND]),
            desc="Sync dotfiles and reload Qtile",
        )
    )


def install_openrouter_widget(config_globals):
    """Install OpenRouter telemetry and related Qtile runtime helpers."""
    _install_sync_and_reload_key(config_globals)

    original_widgets = config_globals.get("widgets")
    separator = config_globals.get("sep")
    home = config_globals.get("home")
    colors = config_globals.get("colors")

    if not callable(original_widgets) or not home or not colors:
        return
    if getattr(original_widgets, "_openrouter_wrapped", False):
        return

    def widgets_with_openrouter():
        items = original_widgets()
        if any(getattr(item, "name", None) == "openrouter_status" for item in items):
            return items

        insert_at = next(
            (
                index + 1
                for index, item in enumerate(items)
                if item.__class__.__name__ == "Net"
            ),
            len(items),
        )
        additions = [_status_widget(home, colors)]
        if callable(separator):
            additions.insert(0, separator(5))
        items[insert_at:insert_at] = additions
        return items

    widgets_with_openrouter._openrouter_wrapped = True
    config_globals["widgets"] = widgets_with_openrouter
