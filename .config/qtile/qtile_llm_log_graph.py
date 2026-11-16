"""Qtile Cairo graph consuming the llm-log API, separate from OpenRouter."""
from __future__ import annotations

from html import escape
import os
from libqtile.widget import base
from notify import notify
from llm_log_graph import paint_graph, shared_timeline


class LLMLogGraph(base._Widget):
    orientations = base.ORIENTATION_HORIZONTAL

    def __init__(self, width=180, *, base_url='http://127.0.0.1:8787', provider='', model='', **config):
        if type(width) is not int or not 96 <= width <= 1024:
            raise ValueError('graph width must be 96..1024')
        self.feed = shared_timeline(base_url, provider, model)
        self._attached = False
        super().__init__(width, **{'name': 'llm_log_graph', **config})
        self.add_callbacks({'Button1': self.advance, 'Button2': self.resume, 'Button3': self.show_details})

    def _configure(self, qtile, bar):
        super()._configure(qtile, bar)
        if not self._attached:
            self.feed.acquire()
            self._attached = True

    def timer_setup(self):
        self.draw()
        self.timeout_add(1, self.timer_setup)

    def draw(self):
        spec, series, stale = self.feed.read()
        self.drawer.clear(self.background or self.bar.background)
        paint_graph(self.drawer.ctx, self.width, self.height, spec.label, series, stale=stale)
        self.draw_at_default_position()

    def advance(self):
        self.feed.advance()
        self.draw()

    def resume(self):
        self.feed.resume()
        self.draw()

    def show_details(self):
        notify('llm-log token timeline', escape(self.feed.details()))

    def finalize(self):
        if self._attached:
            self.feed.release()
            self._attached = False
        super().finalize()


def install_llm_log_graph(config_globals):
    """Place the NEW graph immediately after GPT; the old graph stays intact."""
    options = {'base_url': os.environ.get('LLM_LOG_API_URL', 'http://127.0.0.1:8787'),
               **config_globals.get('llm_log_graph_options', {})}
    visited = set()
    for screen in config_globals.get('screens', ()):
        for position in ('top', 'bottom'):
            target = getattr(screen, position, None)
            if target is None or id(target) in visited:
                continue
            visited.add(id(target))
            widgets = getattr(target, 'widgets', None)
            if widgets is None or any(getattr(w, 'name', '') == 'llm_log_graph' for w in widgets):
                continue
            index = next((i + 1 for i, w in enumerate(widgets)
                          if getattr(w, 'name', '') == 'llm_quota_gpt'), None)
            if index is not None:
                widgets.insert(index, LLMLogGraph(**options))
