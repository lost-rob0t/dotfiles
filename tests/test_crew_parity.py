import re
from pathlib import Path
import unittest
ROOT = Path(__file__).resolve().parents[1]
class ParityTests(unittest.TestCase):
    def test_generated_sources_match_org(self):
        count = 0
        for name in ("crew.org", "crew-support.org"):
            source = ROOT / "lisp/llm" / name
            blocks = re.findall(r"^#\+begin_src \S+ :tangle (\S+)\n(.*?)^#\+end_src$", source.read_text(), re.M | re.S)
            for relative, content in blocks:
                count += 1
                target = (source.parent / relative).resolve()
                self.assertTrue(target.is_relative_to(ROOT))
                self.assertEqual(target.read_text(), content, str(target))
        self.assertEqual(count, 9)
if __name__ == "__main__": unittest.main()
