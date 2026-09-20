import importlib.util
import json
from pathlib import Path
import unittest
ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("client", ROOT / "scripts/crew-client.py")
CLIENT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CLIENT)
ENV = {"OPENCODE_EMACS_AGENT":"crew-test-1","OPENCODE_CREW_NAME":"test",
       "OPENCODE_EMACS_WORKSPACE":"workspace1","OPENCODE_CREW_RUN":"run1",
       "OPENCODE_EMACS_GENERATION":"7","OPENCODE_CREW_EPOCH":"2"}
class ClientTests(unittest.TestCase):
    def test_message_identity_and_unicode(self):
        p=CLIENT.build({"action":"message","target":"crew-test-2","text":"λ\n\"literal"},ENV)
        self.assertEqual(p["protocol"],"ZARA-CREW/1")
        self.assertEqual(p["generation"],7)
        self.assertEqual(p["text"],"λ\n\"literal")
    def test_spawn(self):
        p=CLIENT.build({"action":"spawn","role":"reviewer","turns":4,"text":"inspect"},ENV)
        self.assertEqual(p["turns"],4)
        self.assertNotIn("to",p)
    def test_no_spoofing(self):
        with self.assertRaises(ValueError):
            CLIENT.build({"action":"spawn","role":"reviewer","text":"inspect","from":"operator"},ENV)
    def test_rejects_bad_budget_and_self(self):
        for request in ({"action":"spawn","role":"reviewer","turns":True,"text":"x"},
                        {"action":"spawn","role":"reviewer","turns":-1,"text":"x"},
                        {"action":"message","target":"crew-test-1","text":"x"},
                        {"action":"advance","text":"approved"},
                        {"action":"spawn","role":"../x","text":"x"}):
            with self.subTest(request=request), self.assertRaises(ValueError): CLIENT.build(request,ENV)
    def test_duplicate_keys_rejected(self):
        with self.assertRaises(ValueError): json.loads('{"text":"a","text":"b"}',object_pairs_hook=CLIENT.no_duplicates)
    def test_requires_host_environment(self):
        with self.assertRaises(ValueError): CLIENT.build({"action":"spawn","role":"x","text":"x"},{})
if __name__ == "__main__": unittest.main()
