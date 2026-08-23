#!/usr/bin/env python3
"""Regression tests for the Qtile-owned OpenRouter collector daemon."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import os
import sys
import tempfile
import time
import unittest
from pathlib import Path
from unittest import mock

QTILE_DIR = Path(__file__).resolve().parents[1]
SCRIPT = QTILE_DIR / "scripts" / "openrouter_status.py"
sys.path.insert(0, str(QTILE_DIR))
SPEC = importlib.util.spec_from_file_location("openrouter_status_daemon", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = MODULE
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class OpenRouterDaemonTests(unittest.TestCase):
    def test_json_is_cache_only_and_never_calls_provider(self):
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.dict(os.environ, {"XDG_CACHE_HOME": directory}, clear=False):
                MODULE._write_cache(
                    MODULE._cache_path(),
                    MODULE.Status(1234, 56, balance_usd=12.5),
                    fetched_at=time.time(),
                    metadata={
                        "collector_pid": os.getpid(),
                        "collector_parent_pid": os.getpid(),
                        "collector_heartbeat": time.time(),
                        "collector_error": None,
                    },
                )
                output = io.StringIO()
                with (
                    mock.patch.object(MODULE, "_ensure_daemon", return_value=False),
                    mock.patch.object(
                        MODULE,
                        "_request_json",
                        side_effect=AssertionError("--json must never touch OpenRouter"),
                    ),
                    contextlib.redirect_stdout(output),
                ):
                    rc = MODULE.main(["--json"])
        self.assertEqual(rc, 0)
        payload = json.loads(output.getvalue())
        self.assertEqual(payload["input_tokens_per_minute"], 1234)
        self.assertEqual(payload["output_tokens_per_minute"], 56)
        self.assertEqual(payload["balance_usd"], 12.5)

    def test_empty_cache_returns_starting_immediately(self):
        with tempfile.TemporaryDirectory() as directory:
            output = io.StringIO()
            with (
                mock.patch.dict(os.environ, {"XDG_CACHE_HOME": directory}, clear=False),
                mock.patch.object(MODULE, "_ensure_daemon", return_value=True) as ensure,
                mock.patch.object(
                    MODULE,
                    "load_management_key",
                    side_effect=AssertionError("cache reader must not load credentials"),
                ),
                contextlib.redirect_stdout(output),
            ):
                rc = MODULE.main(["--json"])
        self.assertEqual(rc, 0)
        ensure.assert_called_once()
        self.assertEqual(json.loads(output.getvalue())["error"], "starting")

    def test_fresh_matching_collector_is_not_spawned_again(self):
        now = time.time()
        cache = {
            "collector_pid": os.getpid(),
            "collector_parent_pid": 4242,
            "collector_heartbeat": now,
        }
        with (
            mock.patch.object(MODULE, "_read_cache", return_value=cache),
            mock.patch.object(MODULE.subprocess, "Popen") as popen,
        ):
            started = MODULE._ensure_daemon(4242, now=now)
        self.assertFalse(started)
        popen.assert_not_called()

    def test_stale_collector_is_relaunched_detached(self):
        now = time.time()
        cache = {
            "collector_pid": 99999999,
            "collector_parent_pid": 4242,
            "collector_heartbeat": now - MODULE.DAEMON_HEARTBEAT_STALE_SECONDS - 1,
        }
        with (
            mock.patch.object(MODULE, "_read_cache", return_value=cache),
            mock.patch.object(MODULE.subprocess, "Popen") as popen,
        ):
            started = MODULE._ensure_daemon(4242, now=now)
        self.assertTrue(started)
        command = popen.call_args.args[0]
        self.assertIn("--daemon", command)
        self.assertIn("--parent-pid", command)
        self.assertIn("4242", command)
        self.assertTrue(popen.call_args.kwargs["start_new_session"])

    def test_widget_payload_surfaces_collector_error_without_network(self):
        status = MODULE.Status(100, 10, balance_usd=7.0)
        cache = {
            "fetched_at": time.time(),
            "status": MODULE.asdict(status),
            "collector_pid": 12,
            "collector_parent_pid": 34,
            "collector_heartbeat": time.time(),
            "collector_error": "management key missing",
        }
        payload = MODULE._cached_json_payload(cache, now=time.time())
        self.assertEqual(payload["input_tokens_per_minute"], 100)
        self.assertEqual(payload["collector_error"], "management key missing")
        self.assertEqual(payload["last_error"], "management key missing")


if __name__ == "__main__":
    unittest.main()
