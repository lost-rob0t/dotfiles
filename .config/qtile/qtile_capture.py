from libqtile.config import Key
from libqtile.lazy import lazy


def install_capture_bindings(config_globals):
    """Install the stable screen-capture bindings once per Qtile config load."""
    keys = config_globals.get("keys")
    if keys is None:
        return

    mod = config_globals.get("mod", "mod4")
    bindings = [
        Key(
            [],
            "Print",
            lazy.spawn("screen-capture screenshot-screen"),
            desc="Capture full screen",
        ),
        Key(
            [mod],
            "Print",
            lazy.spawn("screen-capture screenshot-region"),
            desc="Capture selected region",
        ),
        Key(
            ["shift"],
            "Print",
            lazy.spawn("screen-capture record-ui"),
            desc="Open screen-recording UI",
        ),
        Key(
            [mod, "shift"],
            "Print",
            lazy.spawn("screen-capture stop"),
            desc="Stop and finalize screen recording",
        ),
    ]

    existing = {
        (tuple(getattr(binding, "modifiers", ())), getattr(binding, "key", None))
        for binding in keys
    }
    for binding in bindings:
        signature = (tuple(binding.modifiers), binding.key)
        if signature not in existing:
            keys.append(binding)
            existing.add(signature)
