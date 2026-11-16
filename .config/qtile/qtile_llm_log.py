"""Adjacent z.AI/GPT quota widgets; all render-time reads are in memory."""
from __future__ import annotations

from html import escape
import os
import re
import time

from libqtile.widget import base
from notify import notify
from llm_log_client import LABELS, details, find_provider, render_quota, shared_feed

PALETTE = {"green": "#57c785", "yellow": "#e5c54b", "red": "#ef6464", "neutral": "#999999"}


class LLMQuota(base.InLoopPollText):
    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, provider_id, *, base_url="http://127.0.0.1:8787", warning=70,
                 critical=90, rotate_seconds=5, palette=None, **config):
        if provider_id not in LABELS or not 0 <= warning < critical <= 100 or not 1 <= rotate_seconds <= 3600:
            raise ValueError("invalid quota widget configuration")
        self.provider_id = provider_id
        self.feed = shared_feed(base_url)
        self.warning, self.critical = warning, critical
        self.rotate_seconds = rotate_seconds
        self.palette = {**PALETTE, **(palette or {})}
        if any(not re.fullmatch(r"#[0-9a-fA-F]{6}", c) for c in self.palette.values()):
            raise ValueError("quota palette must use #RRGGBB colors")
        self.index, self.paused, self._attached = 0, False, False
        self.changed_at = time.monotonic()
        defaults = {"update_interval": 1, "markup": True, "fontsize": 12, "padding": 5,
                    "name": f"llm_quota_{provider_id}"}
        super().__init__(f"{LABELS[provider_id]} …", **{**defaults, **config})
        self.add_callbacks({"Button1": self.advance, "Button2": self.resume, "Button3": self.show_details})

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
        item = find_provider(snapshot, self.provider_id)
        count = len(item.windows) if item else 0
        now = time.monotonic()
        if not self.paused and count and now - self.changed_at >= self.rotate_seconds:
            steps = int((now - self.changed_at) // self.rotate_seconds)
            self.index = (self.index + steps) % count
            self.changed_at += steps * self.rotate_seconds
        text, level = render_quota(snapshot, self.provider_id, self.index, time.time(),
                                   offline=failed, warning=self.warning, critical=self.critical)
        return f'<span foreground="{self.palette[level]}">{escape(text)}</span>'

    def advance(self):
        self.index += 1
        self.paused = True
        self.changed_at = time.monotonic()
        self.update(self.poll())

    def resume(self):
        self.paused = False
        self.changed_at = time.monotonic()
        self.update(self.poll())

    def show_details(self):
        snapshot, failed = self.feed.read()
        notify(f"{LABELS[self.provider_id]} quota", escape(details(snapshot, self.provider_id,
                                                                 time.time(), offline=failed)))


def make_quota_widgets(**options):
    options = {"base_url": os.environ.get("LLM_LOG_API_URL", "http://127.0.0.1:8787"), **options}
    return [LLMQuota("zai", **options), LLMQuota("gpt", **options)]


def install_llm_log_widgets(config_globals):
    """Insert one adjacent pair, once per horizontal bar; retain other widgets."""
    options = config_globals.get("llm_log_widget_options", {})
    visited = set()
    for screen in config_globals.get("screens", ()):
        for position in ("top", "bottom"):
            target = getattr(screen, position, None)
            if target is None or id(target) in visited:
                continue
            visited.add(id(target))
            widgets = getattr(target, "widgets", None)
            if widgets is None or any(getattr(w, "name", "") == "llm_quota_zai" for w in widgets):
                continue
            index = next((i for i, w in enumerate(widgets) if getattr(w, "name", "") == "clock"), len(widgets))
            widgets[index:index] = make_quota_widgets(**options)
