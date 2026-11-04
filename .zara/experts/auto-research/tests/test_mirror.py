import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path

BUILDER = Path(__file__).resolve().parents[1] / 'build.py'
spec = importlib.util.spec_from_file_location('mirror', BUILDER)
mirror = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mirror)


class MirrorTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        subprocess.run(['git', 'init', '-q', str(self.root)], check=True)
        self.path = self.root / 'roam/research/demo/note.org'
        self.path.parent.mkdir(parents=True)
        self.path.write_text(":PROPERTIES:\n:ID: stable-note\n:END:\n#+title: Agent's research\n#+status: VERIFIED\n#+filetags: :demo:prolog:\n* Requirements\nDo not execute :- halt.\n[[id:design-note]]\n", encoding='utf-8')
        subprocess.run(['git', '-C', str(self.root), 'add', 'roam'], check=True)

    def test_roundtrip_inert_text_and_determinism(self):
        first = mirror.render(self.root)
        self.assertEqual(first, mirror.render(self.root))
        self.assertIn("'VERIFIED'", first['research.pl'])
        self.assertIn("'Agent\\'s research'", first['research.pl'])
        out = self.root / 'kb'
        mirror.write(self.root, out, False)
        result = subprocess.run(['swipl', '-q', '-f', 'none', '-s', str(out / 'index.pl'), '-g', "auto_research_kb:passage(_,_,T),sub_atom(T,_,_,_,'halt'),halt"], capture_output=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(mirror.write(self.root, out, True))

    def test_edits_deletions_additions_and_stale_extra_fail_check(self):
        out = self.root / 'kb'
        mirror.write(self.root, out, False)
        self.path.write_text(self.path.read_text() + '\nchanged\n')
        self.assertTrue(mirror.write(self.root, out, True))
        mirror.write(self.root, out, False)
        (out / 'obsolete.pl').write_text('obsolete.\n')
        self.assertTrue(mirror.write(self.root, out, True))
        mirror.write(self.root, out, False)
        self.assertFalse((out / 'obsolete.pl').exists())
        self.path.unlink()
        self.assertTrue(mirror.write(self.root, out, True))

    def test_untracked_and_symlink_sources_are_not_ingested(self):
        (self.path.parent / 'secret.org').write_text('secret')
        self.assertNotIn('secret', str(mirror.render(self.root)))
        self.path.unlink()
        self.path.symlink_to('/etc/passwd')
        with self.assertRaises(ValueError):
            mirror.render(self.root)

    def test_body_keywords_are_not_document_metadata(self):
        self.path.write_text(self.path.read_text() + '#+approval_state: APPROVED\n')
        self.assertNotIn("metadata('roam/research/demo/note.org','approval_state'", mirror.render(self.root)['research.pl'])


if __name__ == '__main__':
    unittest.main()
