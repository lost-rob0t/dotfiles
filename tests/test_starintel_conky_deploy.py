import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class DeploymentTests(unittest.TestCase):
    def test_literate_parity(self):
        source = (ROOT / 'scripts/starintel-conky.org').read_text()
        blocks = re.findall(r'^#\+begin_src \S+ :tangle (\S+)[^\n]*\n(.*?)^#\+end_src', source, re.M | re.S | re.I)
        self.assertEqual(len(blocks), 3)
        for target, content in blocks:
            self.assertEqual((ROOT / 'scripts' / target).resolve().read_text(), content, target)

    def test_immutable_canonical_pin(self):
        pin = json.loads((ROOT / '.config/starintel-conky-release.json').read_text())
        self.assertIn('git.starintel.actor', pin['repository'])
        self.assertRegex(pin['revision'], r'^[0-9a-f]{40}$')

    def test_launcher_uses_installed_release_and_passes_args(self):
        with tempfile.TemporaryDirectory() as tmp:
            fake = Path(tmp) / 'starintel-conky/current/bin/starintel-conky'
            fake.parent.mkdir(parents=True)
            fake.write_text('#!/bin/sh\nprintf "%s\\n" "$@"\n')
            fake.chmod(0o755)
            result = subprocess.run([str(ROOT / '.local/bin/starintel-conky'), 'status'],
                                    env=dict(os.environ, XDG_DATA_HOME=tmp), capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, 'status\n')

    def test_login_startup_only_starts_starintel(self):
        source = (ROOT / '.config/qtile/scripts/autostart.sh').read_text()
        self.assertIn('"$HOME/.local/bin/starintel-conky" start >/dev/null 2>&1 &', source)
        self.assertNotIn('conky-rotate" --all', source)
        self.assertEqual(source.count('# BEGIN StarIntel desktop'), 1)


if __name__ == '__main__':
    unittest.main()
