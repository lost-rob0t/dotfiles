#!/usr/bin/env python3
"""Minimal MCP stdio adapter for Brave Search CLI (bx).

The server deliberately exposes a small, typed set of bx subcommands instead
of arbitrary argv or shell execution.  Authentication remains bx's concern via
BRAVE_SEARCH_API_KEY or its normal user config file.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from typing import Any


MCP_PROTOCOL_VERSION = "2024-11-05"
SERVER_INFO = {"name": "brave-search-cli-mcp", "version": "0.1.0"}
FRESHNESS_VALUES = {"pd", "pw", "pm", "py"}

TOOLS: list[dict[str, Any]] = [
    {
        "name": "brave_context",
        "description": (
            "Search the web with Brave and return pre-extracted, relevance-scored "
            "grounding content. Prefer this for agent research and RAG."
        ),
        "inputSchema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "query": {"type": "string", "minLength": 1},
                "max_tokens": {
                    "type": "integer",
                    "minimum": 128,
                    "maximum": 32768,
                    "default": 4096,
                },
                "max_urls": {"type": "integer", "minimum": 1, "maximum": 20},
                "max_tokens_per_url": {
                    "type": "integer",
                    "minimum": 128,
                    "maximum": 8192,
                },
            },
            "required": ["query"],
        },
    },
    {
        "name": "brave_web",
        "description": "Run a traditional Brave web search and return structured JSON results.",
        "inputSchema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "query": {"type": "string", "minLength": 1},
                "count": {"type": "integer", "minimum": 1, "maximum": 20, "default": 10},
                "freshness": {
                    "type": "string",
                    "enum": ["pd", "pw", "pm", "py"],
                    "description": "Past day, week, month, or year.",
                },
            },
            "required": ["query"],
        },
    },
    {
        "name": "brave_news",
        "description": "Search Brave News with an optional freshness window.",
        "inputSchema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "query": {"type": "string", "minLength": 1},
                "count": {"type": "integer", "minimum": 1, "maximum": 20, "default": 10},
                "freshness": {
                    "type": "string",
                    "enum": ["pd", "pw", "pm", "py"],
                    "description": "Past day, week, month, or year.",
                },
            },
            "required": ["query"],
        },
    },
    {
        "name": "brave_images",
        "description": "Search Brave Images and return structured image metadata.",
        "inputSchema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "query": {"type": "string", "minLength": 1},
                "count": {"type": "integer", "minimum": 1, "maximum": 200, "default": 20},
            },
            "required": ["query"],
        },
    },
    {
        "name": "brave_places",
        "description": "Search Brave Places/POIs, optionally biased to a textual location.",
        "inputSchema": {
            "type": "object",
            "additionalProperties": False,
            "properties": {
                "query": {"type": "string", "minLength": 1},
                "location": {"type": "string", "minLength": 1},
            },
            "required": ["query"],
        },
    },
]


class ToolInputError(ValueError):
    pass


class BxError(RuntimeError):
    def __init__(self, returncode: int, stderr: str):
        self.returncode = returncode
        self.stderr = stderr
        super().__init__(f"bx exited with status {returncode}")


def require_query(arguments: dict[str, Any]) -> str:
    value = arguments.get("query")
    if not isinstance(value, str) or not value.strip():
        raise ToolInputError("query must be a non-empty string")
    return value


def bounded_int(
    arguments: dict[str, Any],
    name: str,
    *,
    default: int | None,
    minimum: int,
    maximum: int,
) -> int | None:
    value = arguments.get(name, default)
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, int):
        raise ToolInputError(f"{name} must be an integer")
    if value < minimum or value > maximum:
        raise ToolInputError(f"{name} must be between {minimum} and {maximum}")
    return value


def optional_string(arguments: dict[str, Any], name: str) -> str | None:
    value = arguments.get(name)
    if value is None:
        return None
    if not isinstance(value, str) or not value.strip():
        raise ToolInputError(f"{name} must be a non-empty string")
    return value


def optional_freshness(arguments: dict[str, Any]) -> str | None:
    value = optional_string(arguments, "freshness")
    if value is not None and value not in FRESHNESS_VALUES:
        raise ToolInputError("freshness must be one of pd, pw, pm, py")
    return value


def tool_argv(name: str, arguments: dict[str, Any]) -> list[str]:
    query = require_query(arguments)

    if name == "brave_context":
        max_tokens = bounded_int(arguments, "max_tokens", default=4096, minimum=128, maximum=32768)
        argv = ["context", query, "--max-tokens", str(max_tokens)]
        max_urls = bounded_int(arguments, "max_urls", default=None, minimum=1, maximum=20)
        if max_urls is not None:
            argv += ["--max-urls", str(max_urls)]
        per_url = bounded_int(
            arguments,
            "max_tokens_per_url",
            default=None,
            minimum=128,
            maximum=8192,
        )
        if per_url is not None:
            argv += ["--max-tokens-per-url", str(per_url)]
        return argv

    if name in {"brave_web", "brave_news"}:
        command = "web" if name == "brave_web" else "news"
        count = bounded_int(arguments, "count", default=10, minimum=1, maximum=20)
        argv = [command, query, "--count", str(count)]
        freshness = optional_freshness(arguments)
        if freshness is not None:
            argv += ["--freshness", freshness]
        return argv

    if name == "brave_images":
        count = bounded_int(arguments, "count", default=20, minimum=1, maximum=200)
        return ["images", query, "--count", str(count)]

    if name == "brave_places":
        argv = ["places", "-q", query]
        location = optional_string(arguments, "location")
        if location is not None:
            argv += ["--location", location]
        return argv

    raise ToolInputError(f"unknown tool: {name}")


def run_bx(argv: list[str]) -> Any:
    binary = os.environ.get("BRAVE_SEARCH_CLI_BIN", "bx")
    try:
        timeout = int(os.environ.get("BRAVE_MCP_TIMEOUT_SECONDS", "45"))
    except ValueError as exc:
        raise ToolInputError("BRAVE_MCP_TIMEOUT_SECONDS must be an integer") from exc

    env = os.environ.copy()
    env.setdefault("NO_COLOR", "1")
    try:
        completed = subprocess.run(
            [binary, *argv],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=timeout,
            env=env,
        )
    except FileNotFoundError as exc:
        raise BxError(127, f"Brave Search CLI not found: {binary}") from exc
    except subprocess.TimeoutExpired as exc:
        raise BxError(124, f"bx timed out after {timeout} seconds") from exc

    if completed.returncode != 0:
        raise BxError(completed.returncode, completed.stderr.strip())

    try:
        return json.loads(completed.stdout)
    except json.JSONDecodeError as exc:
        raise BxError(1, "bx returned non-JSON output") from exc


def tool_result(name: str, arguments: dict[str, Any]) -> dict[str, Any]:
    try:
        data = run_bx(tool_argv(name, arguments))
    except (ToolInputError, BxError) as exc:
        if isinstance(exc, BxError):
            detail = exc.stderr[:4000] if exc.stderr else str(exc)
            message = f"Brave Search CLI failed (exit {exc.returncode}): {detail}"
        else:
            message = str(exc)
        return {
            "content": [{"type": "text", "text": message}],
            "isError": True,
        }

    return {
        "content": [
            {
                "type": "text",
                "text": json.dumps(data, ensure_ascii=False, separators=(",", ":")),
            }
        ],
        "isError": False,
    }


def response(request_id: Any, result: Any) -> dict[str, Any]:
    return {"jsonrpc": "2.0", "id": request_id, "result": result}


def error_response(request_id: Any, code: int, message: str) -> dict[str, Any]:
    return {
        "jsonrpc": "2.0",
        "id": request_id,
        "error": {"code": code, "message": message},
    }


def handle(message: dict[str, Any]) -> dict[str, Any] | None:
    method = message.get("method")
    request_id = message.get("id")

    # Notifications never receive a response.
    if request_id is None:
        return None

    if method == "initialize":
        return response(
            request_id,
            {
                "protocolVersion": MCP_PROTOCOL_VERSION,
                "capabilities": {"tools": {"listChanged": False}},
                "serverInfo": SERVER_INFO,
            },
        )

    if method == "ping":
        return response(request_id, {})

    if method == "tools/list":
        return response(request_id, {"tools": TOOLS})

    if method == "tools/call":
        params = message.get("params")
        if not isinstance(params, dict):
            return error_response(request_id, -32602, "params must be an object")
        name = params.get("name")
        arguments = params.get("arguments", {})
        if not isinstance(name, str) or not isinstance(arguments, dict):
            return error_response(request_id, -32602, "invalid tool call")
        return response(request_id, tool_result(name, arguments))

    return error_response(request_id, -32601, f"method not found: {method}")


def main() -> int:
    for raw_line in sys.stdin:
        if not raw_line.strip():
            continue
        try:
            message = json.loads(raw_line)
            if not isinstance(message, dict):
                raise ValueError("JSON-RPC message must be an object")
            result = handle(message)
        except (json.JSONDecodeError, ValueError) as exc:
            result = error_response(None, -32700, str(exc))

        if result is not None:
            sys.stdout.write(json.dumps(result, separators=(",", ":")) + "\n")
            sys.stdout.flush()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
