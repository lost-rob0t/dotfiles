"""Read-only Zara search tool for the live public StarIntel API.

The public route proves transport and response shape. Search result quality is
tracked separately; this plugin does not expose unverified write operations.
"""

from __future__ import annotations

import json
import os
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen

from langchain_core.tools import tool


DEFAULT_BASE_URL = "https://ingest.starintel.actor"
DEFAULT_TIMEOUT_SECONDS = 10.0


def _base_url() -> str:
    return os.getenv("STARINTEL_URL", DEFAULT_BASE_URL).rstrip("/")


def _timeout_seconds() -> float:
    try:
        value = float(os.getenv("STARINTEL_TIMEOUT_SECONDS", str(DEFAULT_TIMEOUT_SECONDS)))
    except ValueError:
        return DEFAULT_TIMEOUT_SECONDS
    return min(max(value, 0.1), 120.0)


def _render(value: object) -> str:
    return json.dumps(value, ensure_ascii=False, sort_keys=True)


@tool("starintel_search")
def starintel_search(query: str, limit: int = 25) -> str:
    """Search the public StarIntel document index without making changes."""
    if not query.strip():
        return _render({"ok": False, "error": "query is required"})
    if not 1 <= limit <= 500:
        return _render({"ok": False, "error": "limit must be between 1 and 500"})

    url = f"{_base_url()}/api/v1/search?{urlencode({'q': query, 'limit': limit})}"
    request = Request(url, headers={"Accept": "application/json"}, method="GET")
    try:
        with urlopen(request, timeout=_timeout_seconds()) as response:
            return _render({"ok": True, "status": response.status, "result": json.load(response)})
    except HTTPError as error:
        return _render({"ok": False, "status": error.code, "error": "StarIntel search rejected the request"})
    except (URLError, TimeoutError, OSError) as error:
        return _render({"ok": False, "error": f"StarIntel search unavailable: {error}"})
    except (UnicodeDecodeError, json.JSONDecodeError):
        return _render({"ok": False, "error": "StarIntel search returned invalid JSON"})


def register_tools(_prolog_engine=None):
    """Zara dynamic plugin entry point."""
    return [starintel_search]
