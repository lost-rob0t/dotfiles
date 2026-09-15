"""Pin/source guard; semantic Nix overlay evaluation is a separate CI step."""
from pathlib import Path
import re
import unittest

class PinTests(unittest.TestCase):
    def test_quota_capable_revision_is_pinned(self):
        source = Path('nix/home-manager/mods/llm.nix').read_text()
        revision = re.search(r'llmLogRevision = "([0-9a-f]{40})";', source).group(1)
        self.assertEqual(revision, '692d3820f638c011fc248a7694ecabd72a6e2895')
        self.assertIn('LLM_LOG_QUOTAS_ENABLED=1', source)
        self.assertIn('${config.codex.package}/bin/codex', source)
        self.assertNotIn('LLM_LOG_CODEX_BIN=${config.programs.codex.package}', source)

if __name__ == '__main__':
    unittest.main()
