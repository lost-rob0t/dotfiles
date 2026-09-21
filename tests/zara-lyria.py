#!/usr/bin/env python3
from __future__ import annotations

import base64
import importlib.util
import json
import os
import stat
import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest import mock


class FakeTool:
    def __init__(self, name, function):
        self.name = name
        self.func = function


def tool(name):
    def decorate(function):
        return FakeTool(name, function)
    return decorate


langchain_core = types.ModuleType("langchain_core")
langchain_tools = types.ModuleType("langchain_core.tools")
langchain_tools.tool = tool
langchain_core.tools = langchain_tools
sys.modules.setdefault("langchain_core", langchain_core)
sys.modules.setdefault("langchain_core.tools", langchain_tools)

REPO_ROOT = Path(__file__).resolve().parents[1]
PLUGIN = REPO_ROOT / "nix/home-manager/files/zarathushtra/plugins/lyria.py"
spec = importlib.util.spec_from_file_location("zara_private_lyria", PLUGIN)
lyria = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(lyria)


class FakeResponse:
    def __init__(self, lines):
        self.lines = lines

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def __iter__(self):
        return iter(self.lines)


class LyriaPluginTest(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.output_dir = Path(self.tempdir.name) / "music"
        self.env = mock.patch.dict(
            os.environ,
            {
                "OPENROUTER_API_KEY": "test-secret-key",
                "LYRIA_OUTPUT_DIR": str(self.output_dir),
            },
            clear=False,
        )
        self.env.start()

    def tearDown(self):
        self.env.stop()
        self.tempdir.cleanup()

    def _response_for(self, payload: bytes, transcript: str = ""):
        encoded = base64.b64encode(payload).decode("ascii")
        cut = 5
        events = [
            {"choices": [{"delta": {"audio": {"data": encoded[:cut]}}}]},
            {
                "choices": [
                    {
                        "delta": {
                            "audio": {
                                "data": encoded[cut:],
                                "transcript": transcript,
                            }
                        }
                    }
                ]
            },
        ]
        lines = [f"data: {json.dumps(event)}\n".encode() for event in events]
        lines.append(b"data: [DONE]\n")
        return FakeResponse(lines)

    def test_clip_uses_private_openrouter_policy_and_writes_private_audio(self):
        captured = {}
        expected_audio = b"fake-mp3-audio\x00\x01"

        def fake_urlopen(request, timeout):
            captured["request"] = request
            captured["timeout"] = timeout
            return self._response_for(expected_audio, "generated words")

        with mock.patch.object(lyria, "urlopen", side_effect=fake_urlopen):
            result = json.loads(
                lyria.lyria_generate_clip.func(
                    "dark neon synthwave",
                    title="Mara Feed Hack",
                    instrumental=True,
                )
            )

        self.assertTrue(result["ok"])
        self.assertEqual(result["model"], lyria.CLIP_MODEL)
        output = Path(result["path"])
        self.assertEqual(output.read_bytes(), expected_audio)
        self.assertEqual(stat.S_IMODE(output.stat().st_mode), 0o600)
        self.assertNotIn("test-secret-key", json.dumps(result))

        request = captured["request"]
        body = json.loads(request.data.decode("utf-8"))
        self.assertEqual(body["model"], lyria.CLIP_MODEL)
        self.assertEqual(body["modalities"], ["text", "audio"])
        self.assertEqual(body["audio"], {"format": "mp3"})
        self.assertTrue(body["stream"])
        self.assertEqual(body["provider"], {"data_collection": "deny"})
        self.assertIn("Instrumental only", body["messages"][0]["content"])
        self.assertEqual(request.get_header("Authorization"), "Bearer test-secret-key")

    def test_missing_key_fails_before_network(self):
        os.environ.pop("OPENROUTER_API_KEY", None)
        with mock.patch.object(lyria, "urlopen") as open_mock:
            result = json.loads(lyria.lyria_generate_song.func("anything"))
        self.assertFalse(result["ok"])
        self.assertIn("OPENROUTER_API_KEY", result["error"])
        open_mock.assert_not_called()

    def test_reference_image_is_embedded_as_data_url(self):
        image = Path(self.tempdir.name) / "cover.png"
        image.write_bytes(b"\x89PNG\r\n\x1a\n" + b"x" * 16)
        captured = {}

        def fake_urlopen(request, timeout):
            captured["body"] = json.loads(request.data.decode("utf-8"))
            return self._response_for(b"music")

        with mock.patch.object(lyria, "urlopen", side_effect=fake_urlopen):
            result = json.loads(
                lyria.lyria_generate_song.func(
                    "match this artwork",
                    image_path=str(image),
                    lyrics="hello world",
                )
            )

        self.assertTrue(result["ok"])
        content = captured["body"]["messages"][0]["content"]
        self.assertEqual(content[0]["type"], "text")
        self.assertIn("Lyrics:\nhello world", content[0]["text"])
        self.assertEqual(content[1]["type"], "image_url")
        self.assertTrue(content[1]["image_url"]["url"].startswith("data:image/png;base64,"))

    def test_require_zdr_adds_provider_constraint(self):
        os.environ["LYRIA_REQUIRE_ZDR"] = "true"
        self.assertEqual(lyria._provider_policy(), {"data_collection": "deny", "zdr": True})


if __name__ == "__main__":
    unittest.main()
