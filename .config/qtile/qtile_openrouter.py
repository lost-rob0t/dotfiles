"""OpenRouter status widget integration for the Qtile bar."""

from libqtile import widget


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


def install_openrouter_widget(config_globals):
    """Insert OpenRouter telemetry immediately after the existing Net widget."""
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
