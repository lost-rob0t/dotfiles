"""Zara tools for the local StarIntel HTTP API.

The plugin is intentionally dependency-free beyond Zara/LangChain itself.
Configure the server with STARINTEL_URL and request timeout with
STARINTEL_TIMEOUT_SECONDS.
"""

from __future__ import annotations

import json
import os
from typing import Any, Dict, List, Optional
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urlencode
from urllib.request import Request, urlopen

from langchain_core.tools import tool


DEFAULT_BASE_URL = "http://127.0.0.1:5000"
DEFAULT_TIMEOUT_SECONDS = 10.0
MAX_BULK_DOCUMENTS = 500


def _base_url() -> str:
    return os.getenv("STARINTEL_URL", DEFAULT_BASE_URL).rstrip("/")


def _timeout_seconds() -> float:
    raw = os.getenv("STARINTEL_TIMEOUT_SECONDS", str(DEFAULT_TIMEOUT_SECONDS))
    try:
        timeout = float(raw)
    except ValueError:
        return DEFAULT_TIMEOUT_SECONDS
    return min(max(timeout, 0.1), 120.0)


def _render(value: Any) -> str:
    return json.dumps(value, ensure_ascii=False, indent=2, sort_keys=True)


def _decode_body(raw: bytes) -> Any:
    if not raw:
        return None
    text = raw.decode("utf-8", errors="replace")
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return text


def _request(
    method: str,
    path: str,
    *,
    query: Optional[Dict[str, Any]] = None,
    payload: Any = None,
) -> str:
    if not path.startswith("/"):
        return _render({"ok": False, "error": "StarIntel path must start with '/'."})

    url = f"{_base_url()}{path}"
    if query:
        clean_query = {key: value for key, value in query.items() if value is not None}
        if clean_query:
            url = f"{url}?{urlencode(clean_query)}"

    body = None
    headers = {"Accept": "application/json"}
    if payload is not None:
        body = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        headers["Content-Type"] = "application/json"

    request = Request(url, data=body, headers=headers, method=method.upper())

    try:
        with urlopen(request, timeout=_timeout_seconds()) as response:
            result = _decode_body(response.read())
            return _render(
                {
                    "ok": True,
                    "status": response.status,
                    "result": result,
                }
            )
    except HTTPError as error:
        return _render(
            {
                "ok": False,
                "status": error.code,
                "error": _decode_body(error.read()),
            }
        )
    except URLError as error:
        return _render({"ok": False, "error": f"StarIntel connection failed: {error.reason}"})
    except TimeoutError:
        return _render({"ok": False, "error": "StarIntel request timed out."})
    except OSError as error:
        return _render({"ok": False, "error": f"StarIntel request failed: {error}"})


def _parse_object(raw: str, name: str) -> Dict[str, Any]:
    value = json.loads(raw)
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be a JSON object")
    return value


def _parse_array(raw: str, name: str) -> List[Any]:
    value = json.loads(raw)
    if not isinstance(value, list):
        raise ValueError(f"{name} must be a JSON array")
    return value


@tool("starintel_health")
def starintel_health() -> str:
    """Check whether the configured StarIntel server process is reachable."""
    return _request("GET", "/health")


@tool("starintel_server_info")
def starintel_server_info() -> str:
    """Return StarIntel server metadata, including spec and dataset information."""
    return _request("GET", "/")


@tool("starintel_get_document")
def starintel_get_document(document_id: str) -> str:
    """Fetch one StarIntel document by its exact document ID."""
    if not document_id.strip():
        return _render({"ok": False, "error": "document_id is required"})
    return _request("GET", f"/document/{quote(document_id, safe='')}")


@tool("starintel_search")
def starintel_search(
    query: str,
    limit: int = 25,
    bookmark: Optional[str] = None,
    sort: Optional[str] = None,
) -> str:
    """Run a StarIntel full-text search using the server's Clouseau/Lucene query syntax."""
    if not query.strip():
        return _render({"ok": False, "error": "query is required"})
    if limit < 1 or limit > 500:
        return _render({"ok": False, "error": "limit must be between 1 and 500"})
    return _request(
        "GET",
        "/search",
        query={"q": query, "limit": limit, "bookmark": bookmark, "sort": sort},
    )


@tool("starintel_get")
def starintel_get(path: str, query_json: str = "{}") -> str:
    """Perform a read-only GET against a StarIntel route, useful for document view endpoints."""
    if not path.startswith("/"):
        return _render({"ok": False, "error": "path must start with '/'"})
    try:
        query = _parse_object(query_json, "query_json")
    except (json.JSONDecodeError, ValueError) as error:
        return _render({"ok": False, "error": str(error)})
    return _request("GET", path, query=query)


@tool("starintel_ingest_document")
def starintel_ingest_document(dtype: str, document_json: str) -> str:
    """Publish one document to StarIntel ingest. This is a write operation but not a delete operation."""
    dtype = dtype.strip()
    if not dtype:
        return _render({"ok": False, "error": "dtype is required"})

    try:
        document = _parse_object(document_json, "document_json")
    except (json.JSONDecodeError, ValueError) as error:
        return _render({"ok": False, "error": str(error)})

    body_dtype = document.get("dtype")
    if body_dtype is None:
        document["dtype"] = dtype
    elif body_dtype != dtype:
        return _render(
            {
                "ok": False,
                "error": f"document dtype {body_dtype!r} does not match route dtype {dtype!r}",
            }
        )

    return _request("POST", f"/new/document/{quote(dtype, safe='')}", payload=document)


@tool("starintel_bulk_ingest")
def starintel_bulk_ingest(documents_json: str) -> str:
    """Publish up to 500 StarIntel documents in one bulk ingest request."""
    try:
        documents = _parse_array(documents_json, "documents_json")
    except (json.JSONDecodeError, ValueError) as error:
        return _render({"ok": False, "error": str(error)})

    if not documents:
        return _render({"ok": False, "error": "documents_json must not be empty"})
    if len(documents) > MAX_BULK_DOCUMENTS:
        return _render(
            {
                "ok": False,
                "error": f"bulk ingest is limited to {MAX_BULK_DOCUMENTS} documents",
            }
        )

    for index, document in enumerate(documents):
        if not isinstance(document, dict):
            return _render({"ok": False, "error": f"document {index} is not an object"})
        if not document.get("dtype"):
            return _render({"ok": False, "error": f"document {index} has no dtype"})

    return _request("POST", "/documents/bulk", payload=documents)


@tool("starintel_create_target")
def starintel_create_target(actor: str, target_json: str) -> str:
    """Publish a StarIntel target for a named actor."""
    actor = actor.strip()
    if not actor:
        return _render({"ok": False, "error": "actor is required"})

    try:
        target = _parse_object(target_json, "target_json")
    except (json.JSONDecodeError, ValueError) as error:
        return _render({"ok": False, "error": str(error)})

    return _request("POST", f"/new/target/{quote(actor, safe='')}", payload=target)


@tool("starintel_list_targets")
def starintel_list_targets(actor: str) -> str:
    """List persisted StarIntel targets for a named actor."""
    actor = actor.strip()
    if not actor:
        return _render({"ok": False, "error": "actor is required"})
    return _request("GET", f"/targets/{quote(actor, safe='')}")


def register_tools(_prolog_engine=None):
    """Entry point used by Zara's dynamic plugin loader."""
    return [
        starintel_health,
        starintel_server_info,
        starintel_get_document,
        starintel_search,
        starintel_get,
        starintel_ingest_document,
        starintel_bulk_ingest,
        starintel_create_target,
        starintel_list_targets,
    ]
