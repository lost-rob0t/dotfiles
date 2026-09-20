"""No provider/network/Emacs dependency; real ERT lives in the companion suite."""
import base64
import importlib.util
import json
from pathlib import Path
import re
import subprocess
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("opencode_peer", ROOT / "scripts/opencode-peer.py")
PEER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PEER)
ENV = {"OPENCODE_EMACS_AGENT": "alice", "OPENCODE_EMACS_WORKSPACE": "workspace1",
       "OPENCODE_EMACS_GENERATION": "7"}


class PeerTests(unittest.TestCase):
    def test_envelope_preserves_unicode_and_quotes(self):
        text = '\" ) (delete-file \"bad\")\nλ 😀'
        value = PEER.envelope("bob", text, ENV)
        self.assertEqual(value["text"], text)
        self.assertEqual(value["generation"], 7)

    def test_rejects_unknown_identity_self_and_oversize(self):
        for target, text, environment in [("bob", "hello", {}), ("alice", "x", ENV),
                                           ("../bob", "x", ENV), ("bob", "", ENV),
                                           ("bob", "😀" * 4097, ENV)]:
            with self.subTest(target=target), self.assertRaises(ValueError):
                PEER.envelope(target, text, environment)

    def test_deliver_uses_private_data_file_and_fixed_expression(self):
        packet = PEER.envelope("bob", 'danger \"; $(touch /tmp/no)\nλ', ENV)
        paths = []
        def run(argv, **kwargs):
            self.assertEqual(argv[:3], ["emacsclient", "--socket-name", "server"])
            self.assertNotIn(packet["text"], argv[-1])
            self.assertNotIn("shell", kwargs)
            encoded = re.search(r'base64-decode-string "([A-Za-z0-9+/=]+)"', argv[-1]).group(1)
            path = Path(base64.b64decode(encoded).decode())
            paths.append(path)
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            self.assertEqual(path.parent.stat().st_mode & 0o777, 0o700)
            self.assertEqual(json.loads(path.read_text()), packet)
            return subprocess.CompletedProcess(argv, 0, "t\n")
        with patch.object(PEER.subprocess, "run", run):
            PEER.deliver(packet)
        self.assertFalse(paths[0].exists())
        self.assertFalse(paths[0].parent.exists())

    def test_failure_cleanup_and_false_receipt(self):
        for result in [subprocess.CompletedProcess([], 1, "t"),
                       subprocess.CompletedProcess([], 0, "nil"),
                       subprocess.CompletedProcess([], 0, '"t"')]:
            with patch.object(PEER.subprocess, "run", return_value=result):
                with self.assertRaises(RuntimeError):
                    PEER.deliver(PEER.envelope("bob", "x", ENV))

    def test_timeout_is_not_success(self):
        with patch.object(PEER.subprocess, "run", side_effect=subprocess.TimeoutExpired("emacsclient", 15)):
            with self.assertRaises(subprocess.TimeoutExpired):
                PEER.deliver(PEER.envelope("bob", "x", ENV))

    def test_literate_parity(self):
        source = ROOT / "lisp/llm/opencode-workspace.org"
        blocks = re.findall(r"^#\+begin_src \S+ :tangle (\S+)\n(.*?)^#\+end_src$",
                            source.read_text(), re.MULTILINE | re.DOTALL | re.IGNORECASE)
        self.assertGreaterEqual(len(blocks), 2)
        for relative, content in blocks:
            path = (source.parent / relative).resolve()
            self.assertTrue(path.is_relative_to(ROOT))
            self.assertEqual(path.read_text(), content, str(path))


if __name__ == "__main__":
    unittest.main()
