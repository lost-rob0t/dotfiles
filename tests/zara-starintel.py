"""Check the public StarIntel search tool without network or provider calls."""

from __future__ import annotations

import importlib.util
import json
import sys
import types
import unittest
from pathlib import Path
from unittest.mock import patch


class Response:
    status = 200

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        return False

    def read(self):
        return b'{"rows": [], "total_rows": 0}'


class StarIntelToolTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        langchain = types.ModuleType("langchain_core")
        tools = types.ModuleType("langchain_core.tools")
        tools.tool = lambda _name: lambda function: function
        sys.modules["langchain_core"] = langchain
        sys.modules["langchain_core.tools"] = tools
        path = Path(__file__).resolve().parents[1] / "nix/home-manager/files/zarathushtra/plugins/starintel.py"
        spec = importlib.util.spec_from_file_location("zara_starintel", path)
        cls.plugin = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.plugin)

    def test_only_public_search_is_exposed(self):
        self.assertEqual([tool.__name__ for tool in self.plugin.register_tools()], ["starintel_search"])

    def test_search_uses_live_public_route(self):
        with patch.dict("os.environ", {"STARINTEL_URL": "https://ingest.starintel.actor"}), patch.object(
            self.plugin, "urlopen", return_value=Response()
        ) as open_url:
            result = json.loads(self.plugin.starintel_search("name:Alice", 3))
        self.assertTrue(result["ok"])
        self.assertEqual(result["result"]["rows"], [])
        request = open_url.call_args.args[0]
        self.assertEqual(request.full_url, "https://ingest.starintel.actor/api/v1/search?q=name%3AAlice&limit=3")
        self.assertEqual(request.get_method(), "GET")


if __name__ == "__main__":
    unittest.main()
