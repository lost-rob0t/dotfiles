#!/usr/bin/env python3

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SERVER = ROOT / "nix" / "packages" / "brave-mcp" / "server.py"


class BraveMcpTest(unittest.TestCase):
    def test_mcp_tools_are_typed_and_forward_only_fixed_bx_argv(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            log_path = tmp_path / "bx.log"
            fake_bx = tmp_path / "bx"
            fake_bx.write_text(
                f"#!{sys.executable}\n"
                "import json, os, sys\n"
                "with open(os.environ['FAKE_BX_LOG'], 'a', encoding='utf-8') as handle:\n"
                "    handle.write(json.dumps(sys.argv[1:]) + '\\n')\n"
                "print(json.dumps({'argv': sys.argv[1:]}))\n",
                encoding="utf-8",
            )
            fake_bx.chmod(0o755)

            requests = [
                {
                    "jsonrpc": "2.0",
                    "id": 1,
                    "method": "initialize",
                    "params": {"protocolVersion": "2024-11-05"},
                },
                {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
                {
                    "jsonrpc": "2.0",
                    "id": 3,
                    "method": "tools/call",
                    "params": {
                        "name": "brave_context",
                        "arguments": {
                            "query": "alpha; echo definitely-not-a-shell",
                            "max_tokens": 512,
                            "max_urls": 3,
                            "max_tokens_per_url": 256,
                        },
                    },
                },
                {
                    "jsonrpc": "2.0",
                    "id": 4,
                    "method": "tools/call",
                    "params": {
                        "name": "brave_web",
                        "arguments": {"query": "nixos unstable", "count": 5, "freshness": "pw"},
                    },
                },
                {
                    "jsonrpc": "2.0",
                    "id": 5,
                    "method": "tools/call",
                    "params": {
                        "name": "brave_news",
                        "arguments": {"query": "OpenCode", "count": 4, "freshness": "pd"},
                    },
                },
                {
                    "jsonrpc": "2.0",
                    "id": 6,
                    "method": "tools/call",
                    "params": {
                        "name": "brave_images",
                        "arguments": {"query": "star map", "count": 12},
                    },
                },
                {
                    "jsonrpc": "2.0",
                    "id": 7,
                    "method": "tools/call",
                    "params": {
                        "name": "brave_places",
                        "arguments": {"query": "coffee", "location": "Columbus OH US"},
                    },
                },
                {
                    "jsonrpc": "2.0",
                    "id": 8,
                    "method": "tools/call",
                    "params": {
                        "name": "brave_web",
                        "arguments": {"query": "bad freshness", "freshness": "forever"},
                    },
                },
            ]

            env = os.environ.copy()
            env["BRAVE_SEARCH_CLI_BIN"] = str(fake_bx)
            env["FAKE_BX_LOG"] = str(log_path)

            completed = subprocess.run(
                [sys.executable, str(SERVER)],
                input="".join(json.dumps(request) + "\n" for request in requests),
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=True,
                env=env,
            )

            responses = [json.loads(line) for line in completed.stdout.splitlines() if line]
            self.assertEqual([item["id"] for item in responses], list(range(1, 9)))
            self.assertEqual(responses[0]["result"]["protocolVersion"], "2024-11-05")

            tool_names = {tool["name"] for tool in responses[1]["result"]["tools"]}
            self.assertEqual(
                tool_names,
                {
                    "brave_context",
                    "brave_web",
                    "brave_news",
                    "brave_images",
                    "brave_places",
                },
            )
            for tool in responses[1]["result"]["tools"]:
                self.assertFalse(tool["inputSchema"]["additionalProperties"])

            forwarded = [
                json.loads(item["result"]["content"][0]["text"])["argv"]
                for item in responses[2:7]
            ]
            self.assertEqual(
                forwarded,
                [
                    [
                        "context",
                        "alpha; echo definitely-not-a-shell",
                        "--max-tokens",
                        "512",
                        "--max-urls",
                        "3",
                        "--max-tokens-per-url",
                        "256",
                    ],
                    ["web", "nixos unstable", "--count", "5", "--freshness", "pw"],
                    ["news", "OpenCode", "--count", "4", "--freshness", "pd"],
                    ["images", "star map", "--count", "12"],
                    ["places", "-q", "coffee", "--location", "Columbus OH US"],
                ],
            )

            self.assertTrue(responses[7]["result"]["isError"])
            executed = [json.loads(line) for line in log_path.read_text(encoding="utf-8").splitlines()]
            self.assertEqual(executed, forwarded)


if __name__ == "__main__":
    unittest.main()
