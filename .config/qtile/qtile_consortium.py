"""Read-only Consortium status widgets for Qtile's primary screen."""
from __future__ import annotations

from html import escape

from libqtile.widget import base
from notify import notify

from consortium_status import details, render_phase, render_runs, shared_feed

PALETTE = {
    "active": "#2de2e6",
    "ok": "#62FF00",
    "warn": "#fba922",
    "error": "#ef6464",
    "neutral": "#999999",
}


class ConsortiumStatus(base.InLoopPollText):
    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, mode, *, status_path=None, palette=None, **config):
        if mode not in ("runs", "phase"):
            raise ValueError("invalid Consortium widget mode")
        self.mode = mode
        self.feed = shared_feed(status_path)
        self.palette = {**PALETTE, **(palette or {})}
        self._attached = False
        defaults = {
            "update_interval": 1,
            "markup": True,
            "fontsize": 12,
            "padding": 5,
            "name": f"consortium_{mode}",
        }
        super().__init__("CONS …" if mode == "runs" else "ADARD …", **{**defaults, **config})
        self.add_callbacks({"Button3": self.show_details})

    def _configure(self, qtile, bar):
        super()._configure(qtile, bar)
        if not self._attached:
            self.feed.acquire()
            self._attached = True

    def finalize(self):
        if self._attached:
            self.feed.release()
            self._attached = False
        super().finalize()

    def poll(self):
        snapshot, failed = self.feed.read()
        text, level = (render_runs(snapshot, failed) if self.mode == "runs"
                       else render_phase(snapshot, failed))
        return f'<span foreground="{self.palette[level]}">{escape(text)}</span>'

    def show_details(self):
        snapshot, failed = self.feed.read()
        notify("Consortium", escape(details(snapshot, failed, self.feed.path)))


def make_consortium_widgets(**options):
    return [ConsortiumStatus("runs", **options), ConsortiumStatus("phase", **options)]


def install_consortium_widgets(config_globals):
    """Install once on the primary screen, immediately before subscription telemetry."""
    screens = config_globals.get("screens", ())
    if not screens:
        return
    screen = screens[0]
    target = getattr(screen, "top", None) or getattr(screen, "bottom", None)
    widgets = getattr(target, "widgets", None) if target is not None else None
    if widgets is None or any(getattr(item, "name", "") == "consortium_runs" for item in widgets):
        return
    index = next((i for i, item in enumerate(widgets) if getattr(item, "name", "") == "llm_quota_zai"), None)
    if index is None:
        index = next((i for i, item in enumerate(widgets) if getattr(item, "name", "") == "clock"), len(widgets))
    widgets[index:index] = make_consortium_widgets()
